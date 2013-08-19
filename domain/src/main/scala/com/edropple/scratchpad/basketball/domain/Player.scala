package com.edropple.scratchpad.basketball.domain

import java.io.File
import scala.collection.mutable.ListBuffer
import com.github.tototoshi.csv.CSVReader
import scala.collection.immutable.ListSet.ListSetBuilder
import com.google.common.base.Strings

case class Player(val id: Int, val name: String);

object Player {
    lazy val PLAYER_COLUMNS: Seq[String] = ActionRecord.AWAY_PLAYER_COLUMNS ++ ActionRecord.HOME_PLAYER_COLUMNS;
    def assemblePlayerMap(files: Seq[File]): Map[String, Player] = {
        val names = new ListSetBuilder[String]()

        files.foreach( f => {
            val reader = CSVReader.open(f);
            reader.allWithHeaders().foreach(row => {
                PLAYER_COLUMNS.foreach(colName => {
                    val value = row.get(colName);
                    if (value.isDefined) {
                        names += value.get.trim;
                    }
                });
            });
        });

        val retList = new ListBuffer[Player];
        var counter = 1;
        names.result().foreach(name => {
            retList += Player(counter, name);
            counter += 1;
        });

        return retList.toSet[Player].map( c => (c.name.toUpperCase, c)).toMap
    }
}
