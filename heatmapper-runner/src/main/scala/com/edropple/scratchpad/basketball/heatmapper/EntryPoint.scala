package com.edropple.scratchpad.basketball.heatmapper

import com.edropple.scratchpad.basketball.domain.Universe
import java.io.File
import com.edropple.scratchpad.basketball.heatmapper.core.Heatmapper
import javax.imageio.ImageIO
import java.awt.image.RenderedImage

object EntryPoint {
//    val players = Seq("Jeff Adrien", "Bismack Biyombo", "DeSagana Diop", "Ben Gordon", "Brendan Haywood",
//                      "Gerald Henderson", "Michael Kidd-Gilchrist", "Josh McRoberts", "Byron Mullens",
//                      "Jannero Pargo", "Ramon Sessions", "Tyrus Thomas", "Kemba Walker", "Reggie Williams");

    val players = Seq("Dwight Howard", "Kobe Bryant", "Steve Nash", "Paul Pierce", "Kevin Garnett");

    def main(args: Array[String]): Unit = {
        val directory = new File("/tmp/12-13");

        val universe = new Universe(directory.listFiles().toSeq);

        val heatmapper = new Heatmapper(universe, false);
        val heatmapperEFG = new Heatmapper(universe, true);

        players.foreach(p => {
            val result = heatmapper.buildHeatmap(p);
            val resultEFG = heatmapperEFG.buildHeatmap(p);

            result.image match {
                case None => println("No image created. Probably an error.");
                case t: Some[RenderedImage] => {
                    val file = new File("/tmp/SCALEDBG5_" + result.player.name.replace(" ", "_") + ".png");
                    ImageIO.write(t.get, "png", file);
                    println("Wrote heatmap for '%s' to '%s'.", result.player.name, file.getAbsolutePath);
                }
            }

            resultEFG.image match {
                case None => println("No image created. Probably an error.");
                case t: Some[RenderedImage] => {
                    val file = new File("/tmp/SCALEDBG5_" + resultEFG.player.name.replace(" ", "_") + "_EFG.png");
                    ImageIO.write(t.get, "png", file);
                    println("Wrote heatmap for '%s' to '%s'.", resultEFG.player.name, file.getAbsolutePath);
                }
            }
        });
    }
}
