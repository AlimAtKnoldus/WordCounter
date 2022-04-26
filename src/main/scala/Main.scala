import SourceReader.SourceReaderEnv

object Main extends zio.App {

  import WordCounter._
  import zio._
  import zio.console._

  val sourceReaderLayer: ZLayer[Any, Nothing, SourceReaderEnv] = SourceReader.live
  val wordCounterLayer: ZLayer[Any, Nothing, WordCounterEnv] = sourceReaderLayer >>> WordCounter.live

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    (for {
     wordCounts <- WordCounter.convert(List("https://zio.dev/version-1.x/datatypes/concurrency/", "https://zio.dev/version-1.x/resources/learning/videos/"))
      .provideLayer(wordCounterLayer)
      _ <- putStrLn(wordCounts)
    } yield ()).exitCode
  }
}
