package com.edropple.scratchpad.basketball.domain

import Team._
import java.io.File
import com.github.tototoshi.csv.CSVReader
import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer
import com.google.common.base.Strings

abstract class ActionRecord(val homePlayers: List[Player], val awayPlayers: List[Player],
                            val period: Int, val time: Int,
                            val teamAction: Team, val homeTeamName: String, val awayTeamName: String);
class ShotRecord(homePlayers: List[Player], awayPlayers: List[Player],
                 period: Int, time: Int,
                 teamAction: Team, homeTeamName: String, awayTeamName: String,
                 val shootingPlayer: Player, val assistingPlayer: Option[Player],
                 val made: Boolean, val points: Int, val shotType: String,
                 val xy: Option[(Int, Int)]) extends ActionRecord(homePlayers, awayPlayers, period, time,
                                                                  teamAction, homeTeamName, awayTeamName)
{
    /**
     * Flips the Y-coordinate for the away team, so as to always be presenting the halfcourt locations.
     */
    lazy val normalizedXY: Option[(Int, Int)] = xy match {
        case None => None;
        case t: Some[(Int, Int)] => {
            teamAction match {
                case Home => Some((t.get._1, Universe.FULLCOURT_LENGTH - t.get._2));
                case Away => t;
            }
        }
    }
}

object ActionRecord {
    lazy val HOME_PLAYER_COLUMNS: Seq[String] = Seq("h1", "h2", "h3", "h4", "h5");
    lazy val AWAY_PLAYER_COLUMNS: Seq[String] = Seq("a1", "a2", "a3", "a4", "a5");

    lazy val FILENAME_PATTERN: Pattern = Pattern.compile("[0-9]{4}(?:-[0-9]{2}){2}-([A-Z]{3})@([A-Z]{3})\\.csv");

    def readFiles(files: Seq[File], players: Map[String, Player]): Seq[ActionRecord] = {
        val list = new ListBuffer[ActionRecord];

        files.foreach(f => {
            readFromFile(f, players, list);
        })

        return list.toSeq;
    }

    def readFromFile(file: File, players: Map[String, Player], list: ListBuffer[ActionRecord]): Unit = {
        val filename = file.getName;
        val matcher = FILENAME_PATTERN.matcher(filename);

        if (!matcher.matches()) throw new Exception("File name does not match pattern: " + filename);

        val away = matcher.group(1);
        val home = matcher.group(2);

        val reader = CSVReader.open(file);

        reader.allWithHeaders().foreach(row => {
            parse(row, players, home, away) match {
                case t: Some[ActionRecord] => list += t.get;
                case None => {}
            }
        });
    }


    def parse(row: Map[String, String], players: Map[String, Player],
              homeTeamName: String, awayTeamName: String): Option[ActionRecord] = {
        val homePlayers: Seq[Player] = HOME_PLAYER_COLUMNS.map(col => getPlayer(col, row, players))
        val awayPlayers: Seq[Player] = AWAY_PLAYER_COLUMNS.map(col => getPlayer(col, row, players))

        val period: Int = row.get("period").getOrElse(throw new Exception("period missing from row" + ", row: " + row)).toInt

        var t: Array[String] = row.get("time").getOrElse(throw new Exception("time missing from row" + ", row: " + row)).trim.split(":")
        val time: Int = t(0).toInt * 60 + t(1).toInt

        val teamAction = row.get("team").getOrElse(throw new Exception("team missing from row" + ", row: " + row)).toUpperCase() match {
                case "OFF" => Team.Officials;
                case `homeTeamName` => Team.Home;
                case `awayTeamName` => Team.Away;
                case unk => throw new Exception("Invalid value in team column: " + unk);
        }


        val nameType = row.get("etype").getOrElse(throw new Exception("etype missing from row" + ", row: " + row))
        nameType match {
            case "shot" => {
                val shootingPlayer: Player = getPlayer("player", row, players);
                val assistingPlayer: Option[Player] = getPlayerOpt("assist", row, players);

                val pText = row.get("points").getOrElse("0");
                val points: Int = if (pText.size == 0) 0 else pText.toInt

                val shotType: String = row.get("type").getOrElse(throw new Exception("shot type missing from row" + ", row: " + row)).trim.toLowerCase;

                val made: Boolean = if (points > 0) {
                    row.get("result").getOrElse(throw new Exception("result column missing" + ", row: " + row)) match {
                        case "made" => true;
                        case "missed" => false;
                        case unknown => throw new Exception("invalid made value: " + unknown + ", row: " + row);
                    }
                } else false;

                val xy: Option[(Int, Int)] = getXY(row);

                return Some(new ShotRecord(homePlayers.toList, awayPlayers.toList, period, time,
                                           teamAction, homeTeamName, awayTeamName,
                                           shootingPlayer, assistingPlayer, made, points, shotType, xy));
            }

            // This can be extended later to add more

            case _ => None
        }
    }


    private def getPlayer(col: String, row: Map[String, String], players: Map[String, Player]): Player = {
        val name = row.get(col).getOrElse(throw new Exception("Not found: " + col + ", row: " + row));

        return players.get(Universe.fixNames(name)).getOrElse(throw new Exception("Player not found: " + name + ", row: " + row));
    }
    private def getPlayerOpt(col: String, row: Map[String, String], players: Map[String, Player]): Option[Player] = {
        val name = row.get(col).getOrElse(throw new Exception("Not found: " + col + ", row: " + row)).toUpperCase();

        return players.get(Universe.fixNames(name));
    }

    private def getXY(row: Map[String, String]): Option[(Int, Int)] = {
        val xOpt = row.get("x");
        val yOpt = row.get("y");

        if (xOpt.isEmpty || xOpt.get.size == 0 || yOpt.isEmpty || yOpt.get.size == 0) return None;

        return Some( (xOpt.get.toInt, yOpt.get.toInt) );
    }
}
