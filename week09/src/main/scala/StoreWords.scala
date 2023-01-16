import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import org.apache.kafka.common.serialization.StringDeserializer
import org.apache.kafka.streams.StreamsConfig

import java.sql.DriverManager
import java.time.Duration
import java.util.Properties
import scala.jdk.CollectionConverters._

object StoreWords extends App {
  val dbConn = DriverManager.getConnection("jdbc:postgresql://localhost:5432/postgres", "postgres", "abc123")

  val props: Properties = new Properties()
  props.put(ConsumerConfig.GROUP_ID_CONFIG, "wordstore-consumer")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
  props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
  props.setProperty("enable.auto.commit", "true")
  props.setProperty("auto.commit.interval.ms", "1000")

  val consumer = new KafkaConsumer(props)
  consumer.subscribe(Seq("words").asJava)
  while(true) {
    val records = consumer.poll(Duration.ofMillis(300))
    println(s"Got ${records.count()} records")
    records.records("words").asScala.foreach{ record =>
      val ps = dbConn.prepareStatement(s"insert into words(word) values('${record.value()}') on conflict do nothing")
      println(ps)
      ps.executeUpdate()
    }
  }
}
