package a01

import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.StreamsBuilder
import org.apache.kafka.streams.scala.serialization.Serdes
import org.apache.kafka.streams.{KafkaStreams, StreamsConfig}
import org.apache.kafka.common.serialization.{Deserializer, Serde, Serializer}
import org.apache.kafka.streams.kstream.Named

import java.nio.charset.StandardCharsets
import java.util.Properties
import java.time.Duration
import scala.util.matching.Regex
import org.apache.kafka.clients.consumer.ConsumerConfig.AUTO_OFFSET_RESET_CONFIG

sealed trait Delta
case object DeltaEmpty extends Delta
case class DeltaPartial(user: String, n: Long, t: Long, x: Double, y: Double) extends Delta
case class DeltaFull(user: String, n: Long, tPrev: Long, tNow: Long, xPrev: Double, xNow: Double, yPrev: Double, yNow: Double) extends Delta

object Delta {
  implicit object DeltaSerde extends Serde[Delta] {
    override def serializer: Serializer[Delta] = (_: String, data: Delta) => data match {
      case DeltaEmpty =>
        "deltaempty".getBytes
      case DeltaPartial(user, n, t, x, y) =>
        s"$user, $n, $t, $x, $y".getBytes
      case DeltaFull(user, n, tPrev, tNow, xPrev, xNow, yPrev, yNow) =>
        s"$user, $n, $tPrev, $tNow, $xPrev, $xNow, $yPrev, $yNow".getBytes
    }

    val DeltaPartialPatternMatcher: Regex = "([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+)".r
    val DeltaFullPatternMatcher = "([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+)".r

    override def deserializer: Deserializer[Delta] = (_: String, data: Array[Byte]) =>
      new String(data, StandardCharsets.UTF_8) match {
        case "deltaempty" => DeltaEmpty
        case DeltaPartialPatternMatcher(user, n, t, x, y) =>
          DeltaPartial(user, n.toLong, t.toLong, x.toDouble, y.toDouble)
        case DeltaFullPatternMatcher(user, n, tPrev, tNow, xPrev, xNow, yPrev, yNow) =>
          DeltaFull(user, n.toLong,
            tPrev.toLong, tNow.toLong, xPrev.toDouble, xNow.toDouble, yPrev.toDouble, yNow.toDouble)
      }
  }
}

case class UserEvent(ts: Long, user: String, x: Double, y: Double)

object UserEvent {
  val UserEventPatternMatcher: Regex = "([^,]+), ([^,]+), ([^,]+), ([^,]+)".r
  implicit object UserEventSerde extends Serde[UserEvent] {
    override def serializer: Serializer[UserEvent] = (_: String, data: UserEvent) => data match {
      case UserEvent(ts, user, x, y) => s"$ts, $user, $x, $y".getBytes
    }

    override def deserializer: Deserializer[UserEvent] = (_: String, data: Array[Byte]) =>
      new String(data, StandardCharsets.UTF_8) match {
        case UserEventPatternMatcher(ts, user, x, y) => UserEvent(ts.toLong, user, x.toDouble, y.toDouble)
      }
  }
}

object ComputeVelocities extends App {
  import Serdes._
  import UserEvent._
  import Delta._
  val props = new Properties()
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "velocity-application")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(AUTO_OFFSET_RESET_CONFIG, "earliest")
  props.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.stringSerde.getClass)
  props.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.stringSerde.getClass)
  props.put(StreamsConfig.CACHE_MAX_BYTES_BUFFERING_CONFIG, 0)

  val builder = new StreamsBuilder
  val userEvents = builder.stream[String, String]("user-events").mapValues{ v => v match {
    case UserEventPatternMatcher(ts, user, x, y) =>
      val output = UserEvent(ts.toLong, user, x.toDouble, y.toDouble)
      println(output)
      output
  } }

  userEvents.groupByKey.aggregate(DeltaEmpty: Delta, Named.as("user-ev-with-prec"))(Function.untupled {
    case (_: String, UserEvent(ts, user, x, y), DeltaEmpty) => DeltaPartial(user, 0L, ts, x, y)
    case (_: String, UserEvent(tsNow, userNow, xNow, yNow), DeltaPartial(userPrev, n, tsPrev, xPrev, yPrev))
      if userPrev == userNow =>
        DeltaFull(userNow, n+1, tsPrev, tsNow, xPrev, xNow, yPrev, yNow)
    case (_: String, UserEvent(tsNow, userNow, xNow, yNow), DeltaFull(userPrev, n, _, tsPrev, _, xPrev, _, yPrev))
      if userPrev == userNow =>
        DeltaFull(userNow, n+1, tsPrev, tsNow, xPrev, xNow, yPrev, yNow)
  }).toStream(Named.as("delta")).mapValues(x => {println(x) ; x}).filter((_, v) => v.isInstanceOf[DeltaFull]).mapValues({
    case DeltaFull(user, n, tsPrev, tsNow, xPrev, xNow, yPrev, yNow) =>
      val dx = xNow - xPrev
      val dy = yNow - yPrev
      val velocity = Math.sqrt(dx * dx + dy * dy) / (tsNow - tsPrev)
      val output = s"$n, $user, $tsNow, $velocity"
      println(output)
      output
  }: Delta => String).to("user-velocity")
  val streams: KafkaStreams = new KafkaStreams(builder.build(), props)
  streams.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }


}
