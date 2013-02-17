/**
 * Created with IntelliJ IDEA.
 * User: joe
 * Date: 16/02/2013
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.sound.midi.Track
import melodysequence.{Midi, Plotter}

object Main {

  def main(args : Array[String]) {
    var track = Midi.getTrackFromMidiFile("/Volumes/Home/tunedb.entire/all/213/21323.mid", 0)
    track match {
      case Some(track: Track) => {
        val structure = Midi.structureFromTrack(track)
        val brackets = List((1, 5, 7, 11), (12, 15, 17, 20), (55, 56, 58, 59), (1, 10, 21, 30), (30, 40, 50, 60))

        val image : BufferedImage = Plotter.plotStructure(structure, brackets)
        ImageIO.write(image, "PNG", new File("/tmp/test.png"))
      }
      case None => {}
    }


  }
}
