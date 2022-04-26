import zio._

import java.io.IOException
import scala.io.{BufferedSource, Source}

object SourceReader {
  type SourceReaderEnv = Has[SourceReader.Service]

  trait Service {
    def readFile(address: String): BufferedSource

    def open(address: String): Managed[IOException, Source]
  }

  val live: ZLayer[Any, Nothing, SourceReaderEnv] = ZLayer.succeed(new Service{
    override def readFile(address: String): BufferedSource = {
      Source.fromURL(address)
    }

    override def open(address: String): Managed[IOException, Source] = {
      val acquire: ZIO[Any, IOException, BufferedSource] = ZIO(readFile(address)).refineToOrDie[IOException]
      val release = (source: Source) => ZIO(source.close()).orDie

      Managed.make(acquire)(release)
    }
  })
}

