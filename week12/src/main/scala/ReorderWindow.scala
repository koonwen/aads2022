import org.apache.kafka.streams.scala.ImplicitConversions._
import org.apache.kafka.streams.scala.{ByteArrayKeyValueStore, StreamsBuilder}
import org.apache.kafka.streams.scala.serialization.Serdes
import org.apache.kafka.streams.{KafkaStreams, StreamsConfig, Topology}
import org.apache.kafka.common.serialization.{Deserializer, Serde, Serializer, StringSerializer}
import org.apache.kafka.clients.consumer.ConsumerConfig.AUTO_OFFSET_RESET_CONFIG
import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerConfig, ProducerRecord}
import org.apache.kafka.streams.processor.StateStore
import org.apache.kafka.streams.scala.kstream.Materialized
import org.apache.kafka.streams.state.Stores

import java.nio.charset.StandardCharsets
import java.util.Properties
import java.time.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random


object ReorderWindow extends App {
  import Serdes._
  val props = new Properties()
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "reorder")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(AUTO_OFFSET_RESET_CONFIG, "earliest")
  props.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, stringSerde.getClass)
  props.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, stringSerde.getClass)
  props.put(StreamsConfig.CACHE_MAX_BYTES_BUFFERING_CONFIG, 0)
  props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put("acks", "all")
  props.put("linger.ms", 1)
  props.put("retries", 0)

  val N = 50 // 10, 20, 30, 40, 50, 60, 70, 45, 25
  Future {
    val producer: Producer[String, String] = new KafkaProducer[String, String](props)
    (1l to 1000000000l).foreach { k =>
      val r = new ProducerRecord("events", 0.toString, (k * 10).toString)
      println((k * 10).toString)
      producer.send(r)
      if (Random.nextInt(5) == 0 && k > 2 * N) {
        val dist = Random.nextInt(2 * N) * 10 - 5
        val v = (k * 10 - dist).toString
        val rOO = new ProducerRecord("events", 0.toString, v)
        println(v)
        producer.send(rOO)
      }
    }
  }

  def makeTopology(inputTopic: String, outputTopic: String): Topology = {
    val builder = new StreamsBuilder
    builder
      .stream[String, String](inputTopic)
      .groupByKey
      .aggregate(""){ (_, v, a) => a match {
        case "" => v
        case _ =>
          val q = a.split(",").map(_.toInt)
          (() match {
            case _ if q.length > N+1 && q(0) > v.toInt => q.drop(1)
            case _ if q.length > N+1 => (q :+ v.toInt).sorted.drop(1)
            case _ => (q :+ v.toInt).sorted
          }).mkString(",")
      }}
      .toStream
      .filter((_, v) => v.split(",").length > N)
      .mapValues((_, v) => v.split(",")(0))
      .to(outputTopic)
    builder.build()
  }

  val streams = new KafkaStreams(makeTopology("events", "ordered"), props)
  streams.start()

  sys.ShutdownHookThread {
    streams.close(Duration.ofSeconds(10))
  }
}
