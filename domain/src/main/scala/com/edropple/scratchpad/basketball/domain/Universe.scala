package com.edropple.scratchpad.basketball.domain

import java.io.File

class Universe(private val files: Seq[File]) {
    val playersByName: Map[String, Player] = Player.assemblePlayerMap(files)
    val playersByIndex: Map[Int, Player] = playersByName.map(Universe.indexMapTransform).toMap

    val allRecords: Seq[ActionRecord] = ActionRecord.readFiles(files, playersByName);


    def getPlayerByName(name: String): Option[Player] = playersByName.get(Universe.fixNames(name));
    def getPlayerById(id: Int): Option[Player] = playersByIndex.get(id);
}

object Universe {
    private def indexMapTransform(t: (String, Player)): (Int, Player) = (t._2.id, t._2)


    val FULLCOURT_WIDTH = 50;
    val FULLCOURT_LENGTH = 94;
    val HALFCOURT_LENGTH = FULLCOURT_LENGTH / 2;



    def fixNames(name: String): String = {
        var n = name.trim.toUpperCase;
        if (n.equals("METTA WORLD")) return "METTA WORLD PEACE";
        if (n.equals("ROGER MASON")) return "ROGER MASON JR.";
        if (n.equals("NANDO DE")) return "NANDO DE COLO";
        if (n.equals("KYLE O")) return "KYLE O'QUINN";
        if (n.equals("JERMAINE O")) return "JERMAINE O'NEAL";
        if (n.equals("SHAQUILLE O")) return "SHAQUILLE O'NEAL";
        if (n.equals("LUC RICHARD")) return "LUC RICHARD MBAH A MOUTE";

        return n;
    }
}
