package a02

import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerConfig, ProducerRecord}
import org.apache.kafka.common.serialization.StringSerializer
import org.apache.kafka.streams.StreamsConfig

import java.util.Properties
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.Random

object GenerateData extends App {
  val firstNames = Source.fromResource("firstnames.txt").getLines.toArray
  val LastNameMatcher = "#[0-9]+ ([^ ]+) .*".r
  val lastNames = Source.fromResource("lastnames.txt").getLines.map{
    case LastNameMatcher(name) => name.toLowerCase.capitalize
  }.toArray
  val streetNames = Source.fromResource("streetnames.txt").getLines.toArray
  val phrases = Source.fromResource("phrases.txt").getLines.toArray
  val userCount = 10

  case class User(userId: Int, name: String, address: String, phoneNo: String, count: Int)
  def genName(): String = s"${firstNames(Random.nextInt(firstNames.length))} ${lastNames(Random.nextInt(lastNames.length))}"
  def genAddress(): String = s"${Random.nextInt(99)+1} ${streetNames(Random.nextInt(streetNames.length))}"
  def genPhoneNo(): String = s"${1000000000L + Random.nextLong(9000000000L)}"

  val users: Array[User] = (1 to userCount).map(User(_, genName(), genAddress(), genPhoneNo(), 0)).toArray

  val props: Properties = new Properties()
  props.put(StreamsConfig.APPLICATION_ID_CONFIG, "join-app")
  props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer].getName)
  props.put(StreamsConfig.CACHE_MAX_BYTES_BUFFERING_CONFIG, 0)

  props.put("acks", "all")
  props.put("linger.ms", 1)
  props.put("retries", 0)

  val producer: Producer[String, String] = new KafkaProducer[String, String](props)
  val userTopic = "users"
  val msgTopic = "user-msgs"

  users.foreach { user =>producer.send(new ProducerRecord[String, String](userTopic, s"${user.userId}", s"$user")) }

  val f1 = Future {
    while(true) {
      Thread.sleep(5000)
      val pickUser = Random.nextInt(userCount)
      val u = users(pickUser)
      users(pickUser) = User(u.userId, u.name, genAddress(), genPhoneNo(), u.count + 1)
      producer.send(new ProducerRecord[String, String](userTopic, s"${u.userId}", s"${users(pickUser)}"))
    }
  }

  val f2 = Future {
    while(true) {
      Thread.sleep(2000)
      val user = users(Random.nextInt(userCount))
      val msg = phrases(Random.nextInt(phrases.length))
      producer.send(new ProducerRecord[String, String](msgTopic, s"${user.userId}", msg))
    }
  }

  Await.ready(f1 zip f2, Duration.Inf)
}
