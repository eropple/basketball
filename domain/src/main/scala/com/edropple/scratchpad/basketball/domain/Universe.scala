package com.edropple.scratchpad.basketball.domain

import java.io.File

class Universe(private val files: Seq[File]) {
    val playersByName: Map[String, Player] = Player.assemblePlayerMap(files)
    val playersByIndex: Map[Int, Player] = playersByName.map(Universe.indexMapTransform).toMap

    val allRecords: Seq[ActionRecord] = ActionRecord.readFiles(files, playersByName);


    def getPlayerByName(name: String): Option[Player] = playersByName.get(name.trim.toUpperCase);
    def getPlayerById(id: Int): Option[Player] = playersByIndex.get(id);
}

object Universe {
    private def indexMapTransform(t: (String, Player)): (Int, Player) = (t._2.id, t._2)


    val FULLCOURT_WIDTH = 50;
    val FULLCOURT_LENGTH = 94;
    val HALFCOURT_LENGTH = FULLCOURT_LENGTH / 2;
}
