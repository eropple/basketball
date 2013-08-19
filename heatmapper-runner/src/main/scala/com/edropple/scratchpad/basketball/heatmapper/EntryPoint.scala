package com.edropple.scratchpad.basketball.heatmapper

import com.edropple.scratchpad.basketball.domain.Universe
import java.io.File
import com.edropple.scratchpad.basketball.heatmapper.core.Heatmapper
import javax.imageio.ImageIO
import java.awt.image.RenderedImage

object EntryPoint {
    def main(args: Array[String]): Unit = {
        val universe = new Universe(Seq(new File("/tmp/2013-06-06-SAN@MIA.csv")));

        val heatmapper = new Heatmapper(universe);

        val result = heatmapper.buildHeatmap("tony parker");

        result.image match {
            case None => println("No image created. Probably an error.");
            case t: Some[RenderedImage] => {
                val file = new File("/tmp/" + result.player.name.replace(" ", "_") + ".png");
                ImageIO.write(t.get, "png", file);
                println("Wrote heatmap for '%s' to '%s'.", result.player.name, file.getAbsolutePath);
            }
        }

        val i = 0;
    }
}
