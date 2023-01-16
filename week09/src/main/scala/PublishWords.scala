import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerConfig, ProducerRecord}
import org.apache.kafka.common.serialization.StringSerializer
import org.apache.kafka.streams.StreamsConfig

import java.util.Properties
import scala.io.Source
import scala.util.Random

object PublishWords extends App {
  val props: Properties = new Properties()
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "wordcount-application")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put("acks", "all")
  props.put("linger.ms", 1)
  props.put("retries", 0)

  val producer: Producer[String, String] = new KafkaProducer[String, String](props)

  val wordSet = Source.fromResource("words.txt").getLines.toArray
  for(i <- 1 to 1000) {
    val line = (1 to 10).map(_ => wordSet(Random.nextInt(wordSet.length))).mkString(" ")
    val rec = new ProducerRecord[String, String]("text-lines", line(0).toString, line)
    producer.send(rec)
    println(line)
    Thread.sleep(100)
  }
  producer.flush()
  producer.close()
}
