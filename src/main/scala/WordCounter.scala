import java.io.IOException
import scala.io.{BufferedSource, Source}

object WordCounter extends zio.App{

  import zio._
  import zio.console._
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (for {
    fiber1 <- convert("https://zio.dev/version-1.x/overview/overview_handling_resources").fork
    fiber2 <- convert("https://zio.dev/version-1.x/overview/overview_basic_concurrency").fork
    countForFiber1 <-  fiber1.join
    countForFiber2 <- fiber2.join
    _ <- putStrLn(countForFiber1)
    _ <- putStrLn(countForFiber2)
  } yield ()).exitCode

  def readFile(address: String): BufferedSource = {
    Source.fromURL(address)
  }

  def open(address: String): Managed[IOException, Source] = {
    val acquire: ZIO[Any, IOException, BufferedSource]  = ZIO(readFile(address)).refineToOrDie[IOException]
    val release: Source => URIO[Any, Unit] = (source: Source) => ZIO(source.close()).orDie
    Managed.make(acquire)(release)
  }

  def convert(address: String): ZIO[Any, IOException, String] = {
    open(address).use {
      source => ZIO(source.getLines.flatMap(_.split("\\W+"))
        .foldLeft(Map.empty[String, Int]){
          (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
        }.foldLeft(0)(_+_._2).toString).refineToOrDie[IOException]
    }
  }
}
