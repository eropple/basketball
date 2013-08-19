package com.edropple.scratchpad.basketball.heatmapper.core

import com.edropple.scratchpad.basketball.domain.{ShotRecord, Player, Universe}
import java.awt.{RenderingHints, Color, Image}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import java.awt.image.{RenderedImage, BufferedImage}
import javax.imageio.ImageIO
import java.io.File

case class HeatmapResult(val player: Player, val image: Option[RenderedImage],
                         val plottedShots: Seq[ShotRecord], val unplottedShots: Seq[ShotRecord]);

class Heatmapper(val universe: Universe, val pixelsPerFoot: Int = 10) {
    def buildHeatmap(name: String): HeatmapResult =
        buildHeatmap(universe.getPlayerByName(name).getOrElse(throw new Exception("player not found: " + name)));
    def buildHeatmap(player: Player): HeatmapResult = {
        val (plottable, unplottable) = getShotLists(player);

        printf("Shots for '%s': %d plottable, %d unplottable.", player.name, plottable.size, unplottable.size);

        val drawables = constructDrawables(plottable);
        val image = Heatmapper.constructImage(drawables, pixelsPerFoot);

        return HeatmapResult(player, image, plottable, unplottable);
    };



    private def constructDrawables(shots: Seq[ShotRecord]): Seq[((Int, Int), (Double, Double))] = {
        val locations = Heatmapper.collectShotLocations(shots);

        var most_shots: Double = 0;
        locations.foreach(t => {
            most_shots = math.max(most_shots, t._2.size);
        });

        val list = new ListBuffer[((Int, Int), (Double, Double))];
        locations.foreach(t => {
            val makes: Double = t._2.count(shot => shot.made);
            val totalShots: Double = t._2.size;

            val drawSize: Double = t._2.size / most_shots; // 1.0 being full size
            val drawColor: Double = makes / totalShots;
            val drawTuple = (drawSize, drawColor);

            list += ((t._1, drawTuple));
        });

        return list.toSeq
    }

    private def getShotLists(player: Player): (Seq[ShotRecord], Seq[ShotRecord]) = {
        val plottable = new ListBuffer[ShotRecord];
        val unplottable = new ListBuffer[ShotRecord]

        getAllShots(player).foreach(shot => {
            if (shot.normalizedXY.isDefined) plottable += shot else unplottable += shot;
        });

        return (plottable, unplottable);
    }

    private def getAllShots(player: Player): Seq[ShotRecord] = {
        val list = new ListBuffer[ShotRecord]

        universe.allRecords.filter( record => record.isInstanceOf[ShotRecord] &&
                                            record.asInstanceOf[ShotRecord].shootingPlayer == player ).foreach(shot => {
            list += shot.asInstanceOf[ShotRecord]
        });

        return list.toSeq
    }
}

object Heatmapper {
    private def collectShotLocations(shots: Seq[ShotRecord]): Map[(Int, Int), Seq[ShotRecord]] = {
        val shotMap = mutable.Map[(Int, Int), Seq[ShotRecord]]();

        shots.foreach(shot => {
            shotMap.get(shot.normalizedXY.get) match {
                case t: Some[Seq[ShotRecord]] => {
                    val current = t.get;
                    shotMap -= shot.normalizedXY.get;
                    shotMap += ( (shot.normalizedXY.get, current :+ shot) );
                }
                case None => shotMap += ((shot.normalizedXY.get, Seq(shot)));
            }

        });

        return shotMap.toMap;
    }


    private def constructImage(drawables: Seq[((Int, Int), (Double, Double))],
                               pixelsPerFoot: Int): Option[RenderedImage] = {
        val width = Universe.FULLCOURT_WIDTH * pixelsPerFoot;
        val height = Universe.HALFCOURT_LENGTH * pixelsPerFoot;

        val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

        val baseImage = ImageIO.read(new File("/tmp/court.png"));

        val graphics = image.createGraphics();
        val rh = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        rh.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        graphics.setRenderingHints(rh);

        graphics.setBackground(new Color(1.0f, 1.0f, 1.0f));
        graphics.clearRect(0, 0, width, height);

        graphics.drawImage(baseImage, 0, 0, null);


        drawables.foreach(drawable => {
            val location = drawable._1;
            val size = drawable._2._1;
            val intensity = drawable._2._2;

            val squareSize: Int = math.round(pixelsPerFoot * size.asInstanceOf[Float]);

            var pX = location._1 * pixelsPerFoot;
            var pY = height - (location._2 * pixelsPerFoot);
            if (size != 1.0)
            {
                val pOff = (pixelsPerFoot - squareSize) / 2;
                pY += pOff;
                pX += pOff;
            }

            graphics.setColor(new Color(intensity.asInstanceOf[Float], 0.0f, 0.0f));
            graphics.fillRect(pX, pY, squareSize, squareSize);

            println(location, (pX, pY));
        })

        return Some(image);
    }
}
