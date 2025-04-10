package preprocessing

import boopickle.*
import boopickle.Default.*
import cats.effect.*
import cats.implicits.*
import domain.Connection
import domain.Graph
import domain.Stop
import fs2.data.csv.decodeUsingHeaders
import fs2.io.file.Files
import fs2.io.file.Flags
import fs2.io.file.Path
import fs2.Pipe
import fs2.Stream
import fs2.text

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.concurrent.duration.*
import scala.util.Using

object CsvToGraph:

  private def showProcessedCount[F[_]: LiftIO: Temporal, A]: Pipe[F, A, A] = stream =>
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

  // this somehow overrides instead of adding new stops with connecitons
  private def createGraph[F[_]]: Pipe[F, Connection, Graph] = stream =>
    stream
      .fold(Map.empty[Stop, Set[Connection]]) { case (graph, busConnectionSample) =>
        graph
          .updatedWith(busConnectionSample.startStop)(valueOpt =>
            (valueOpt.getOrElse(Set.empty) + busConnectionSample).some
          )
          .updatedWith(busConnectionSample.endStop)(valueOpt => valueOpt.getOrElse(Set.empty).some)
      }

  private def cacheObjects(path: Path): Pipe[IO, Graph, Graph] = stream =>
    stream.evalTap { connections =>
      val bytes = Pickle.intoBytes(connections)
      IO.fromTry(
        Using(new FileOutputStream(path.toString)) { fos =>
          fos.write(bytes.array())
        }
      )
    }

  private def readSerialized[T: Pickler](path: Path): IO[T] =
    Files[IO].readAll(path).compile.to(Array).map { bytes =>
      Unpickle[T].fromBytes(ByteBuffer.wrap(bytes))
    }

  private def readAndCache(data: String, cachePath: Path) =
    Files[IO]
      .readAll(Path(data), 1024, Flags.Read)
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[Connection]())
      .through(showProcessedCount)
      .through(createGraph)
      .through(cacheObjects(cachePath))
      .compile
      .lastOrError

  def getCachedGraphOrReadAndCache(data: String, cachePath: Path): IO[Graph] =
    readSerialized[Graph](cachePath).handleErrorWith(_ => readAndCache(data, cachePath))
