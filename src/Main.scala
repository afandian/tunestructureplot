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
        var structure = Midi.structureFromTrack(track)

        var image : BufferedImage = Plotter.plotStructure(structure)
        ImageIO.write(image, "PNG", new File("/tmp/test.png"))
      }
      case None => {}
    }


  }
}
