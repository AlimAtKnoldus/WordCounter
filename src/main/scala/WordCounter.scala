import zio._

import java.io.IOException

object WordCounter  {
  type WordCounterEnv = Has[WordCounter.Service]

  class Service(reader: SourceReader.Service) {
    def convert(address: List[String]): Task[String] = {
      ZIO.foreach(address) { addresses =>
        reader.open(addresses).use {
          source => ZIO(source.getLines.flatMap(_.split("\\W+"))
            .foldLeft(Map.empty[String, Int]){
              (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
            }).refineToOrDie[IOException]
        }
      }.map { listOfCountsWithWords =>
        listOfCountsWithWords.flatten.groupBy(_._1).mapValues(_.map(_._2).sum).mkString("\n")
      }.refineToOrDie[IOException]
    }
  }

  val live: ZLayer[Has[SourceReader.Service], Nothing, WordCounterEnv] = ZLayer.fromService[SourceReader.Service, WordCounter.Service]{ sourceReader =>
    new Service(sourceReader)
  }

  def convert(address: List[String]): ZIO[WordCounterEnv, Throwable, String] =
    ZIO.accessM(_.get.convert(address))
}
