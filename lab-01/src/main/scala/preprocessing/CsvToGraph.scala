package preprocessing

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import scala.concurrent.duration._
import scala.util.Try

import cats.effect._
import cats.implicits._

import domain.BusConnectionSample
import domain.Graph
import domain.Stop
import fs2.Pipe
import fs2.Stream
import fs2.data.csv.decodeUsingHeaders
import fs2.io.file.Files
import fs2.io.file.Flags
import fs2.io.file.Path
import fs2.text

object CsvToGraph:

  private def showProcessedCount[F[_]: LiftIO: Concurrent: Temporal: Clock, A]: Pipe[F, A, A] = stream =>
    Stream.eval(Ref[F].of(0)).flatMap { count =>
      stream
        .chunks
        .evalMap { chunk =>
          count.update(_ + chunk.size).as(chunk)
        }
        .flatMap(Stream.chunk)
        .concurrently(
          Stream
            .awakeEvery[F](1.second)
            .zip(Stream.repeatEval(count.get))
            .evalMap { case (dur, count) =>
              LiftIO[F].liftIO(IO.println(s"Processed $count elements in ${dur.toSeconds} seconds"))
            }
        )
    }

  private def createGraph[F[_]]: Pipe[F, BusConnectionSample, Graph] = stream =>
    stream
      .fold(Map.empty[Stop, Set[BusConnectionSample]]) { case (graph, busConnectionSample) =>
        graph
          .updatedWith(busConnectionSample.startStop)(valueOpt => (valueOpt.getOrElse(Set.empty) + busConnectionSample).some)
          .updatedWith(busConnectionSample.endStop)(_.getOrElse(Set.empty).some)
      }

  private def serialize(filePath: Path, obj: AnyRef): IO[Unit] =
    IO {
      val file = filePath.toNioPath.toFile
      if (!file.exists()) file.createNewFile()
      val fos = new FileOutputStream(filePath.toString)
      val oos = new ObjectOutputStream(fos)
      try oos.writeObject(obj)
      finally {
        oos.close()
        fos.close()
      }
    }

  private def cacheObjects(path: Path): Pipe[IO, Graph, Graph] = stream => stream.evalTap(connections => serialize(path, connections))

  private def readSerialized[T](path: Path): Option[T] =
    Try(new ObjectInputStream(new FileInputStream(path.toString)).readObject().asInstanceOf[T]).toOption

  private def readAndCache(data: String, cachePath: Path) =
    Files[IO]
      .readAll(Path(data), 1024, Flags.Read)
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[BusConnectionSample]())
      .through(showProcessedCount)
      .through(createGraph)
      .through(cacheObjects(cachePath))
      .compile
      .lastOrError

  def getCachedGraphOrReadAndCache(data: String, cachePath: Path): IO[Graph] = {
    val a = readSerialized[Graph](cachePath)
    a.fold(readAndCache(data, cachePath))(_.pure[IO])
  }
