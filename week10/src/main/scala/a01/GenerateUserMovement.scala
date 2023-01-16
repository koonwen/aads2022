package a01

import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerRecord}
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.common.serialization.StringSerializer

import java.util.Properties
import scala.util.Random
import java.time.{Clock, ZoneId}

object GenerateUserMovement extends App {
  val props: Properties = new Properties()
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "wordcount-application")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put("acks", "all")
  props.put("linger.ms", 1)
  props.put("retries", 0)

  val producer: Producer[String, String] = new KafkaProducer[String, String](props)
  val userCount = Random.nextInt(90) + 10
  val topic = "user-events"
  val eventCount = 100000

  Iterator.iterate(Clock.tickMillis(ZoneId.systemDefault()).millis(), eventCount+1) { time =>
    val user = Random.nextInt(userCount)
    val x = Random.nextInt(2001) - 1000
    val y = Random.nextInt(2001) - 1000
    val output = s"$time, user$user, $x, $y"
    producer.send(new ProducerRecord[String, String](topic, s"$user", output))
    println(output)
    time + Random.nextInt(4) + 1
  }.drop(eventCount+1).nextOption()
  producer.flush()
  producer.close()
}

