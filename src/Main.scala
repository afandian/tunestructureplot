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
    val butterfly = "/Volumes/Home/tunedb.entire/all/213/21323.mid"
    val oldFavourite = "/Volumes/Home/tunedb.entire/all/985/98555.mid"
    val eog = "/Volumes/Home/tunedb.entire/all/155/964/155964.mid"

    var track = Midi.getTrackFromMidiFile(eog, 0)


    track match {
      case Some(track: Track) => {
        val structure = Midi.structureFromTrack(track)
        val tune = structure.asMonophonic()
        val identities = melodysequence.Functions.tuneIdentities(tune)//.slice(0, 20)
        var allPrefixes = melodysequence.Functions.allPrefixesWithSkips(identities.toList, 5)

        // Kludge. Remove short ones.
        allPrefixes = allPrefixes.filter{case(a: Int, b: Int, length: Int) => length > 3}
        val brackets = allPrefixes.map {case(a: Int, b: Int, length: Int) => (a, a + length - 1 , b, b + length - 1)}
        val image : BufferedImage = Plotter.plotStructure(tune, brackets)
        ImageIO.write(image, "PNG", new File("/tmp/eog.png"))
      }
      case None => {}
    }


  }
}
