package pl.edu.agh.mock.utils

import java.io.{FileInputStream, IOException}

import pl.edu.agh.mock.model
import pl.edu.agh.mock.model.TileType.{Empty, Obstacle}
import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}
import play.api.libs.json.{JsError, JsResult, JsString, JsSuccess, JsValue, Json, Reads}

object JsonMapParser {
  implicit val tileTypeObstacleReader: Reads[TileType.Obstacle.type] = Json.reads[TileType.Obstacle.type]
  implicit val tileTypeEmptyReader: Reads[TileType.Empty.type] = Json.reads[TileType.Empty.type]
  implicit val tileTypeReader: Reads[TileType.EnumVal] = {
    case JsString("Empty") => JsSuccess(Empty)
    case JsString("Obstacle") => JsSuccess(Obstacle)
    case _ => JsError("Bad cell type")
  }
  implicit val tileReader: Reads[Tile] = Json.reads[Tile]
  implicit val mapReader: Reads[SimulationMap] = Json.reads[SimulationMap]

  def parseMapFromFile(filename: String): SimulationMap = {
    val stream = new FileInputStream(filename)
    val jsonMap : JsValue = try {
      Json.parse(stream)
    } catch {
      case e : Throwable =>
        println(e.getMessage)
        null
    }
    finally {
      stream.close()
    }
    Json.fromJson[SimulationMap](jsonMap).get
  }
}


