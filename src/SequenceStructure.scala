/**
 * User: joe
 * Date: 16/02/2013
 * Time: 14:07
 */

package melodysequence {

import javax.sound.midi._
import java.io.File
import scala._
import collection.mutable
import collection.mutable.{ListBuffer, Map}
import java.awt.image.BufferedImage
import java.awt._
import java.awt.geom.{Point2D, CubicCurve2D, Path2D}
import scala.Tuple3
import scala.Some
import scala.List

// TODO tail call!

// A melody structure.
class MelodyStructure(events: List[Tuple3[Symbol, Long, Int]]) {

  // Convert the list of note on and off durations into a monophonic sequence
  // of (onset, duration, pitch). In the case of polyphony, last one wins!
  def asMonophonic(): Seq[Tuple3[Long, Long, Int]] = {

    // Some things are best looped!
    // TODO maybe this would be best recursive.
    var lastOnPitch: Int = -1
    var lastOnset: Long = 0
    var events = ListBuffer[Tuple3[Long, Long, Int]]()

    for (event <- this.events) {
      event match {
        case ('NoteOn, time, pitch) => {
          var duration = if (lastOnset == -1) 0; else time - lastOnset

          if (lastOnPitch != -1) {
            // Argh! We've had two note-ons!
            // Append the old one first.
            events.append((time, duration, lastOnPitch))
          }

          lastOnPitch = pitch
          lastOnset = time
        };
        case ('NoteOff, time, pitch) => {

          var duration = if (lastOnset == -1) 0; else time - lastOnset

          if (lastOnPitch == pitch) {
            events.append((lastOnset, duration, pitch))
            lastOnPitch = -1
            lastOnset = -1
          } else {
            // If we're note-offing a pitch that we didn't note-on, just ignore
            // as the other clause will have taken care of it.
            lastOnPitch = -1
            lastOnset = -1
          }
        }
      }
    }

    return events
  }
}

//
object Plotter {
  // Return the modal note duration.
  // This is designed for tunes with lots of identical note lengths.
  def modalDuration(inp: Seq[Tuple3[Long, Long, Int]]) : Double = {
    val lengths = Map[Double, Int]()
    for (note <- inp) {
      lengths.put(note._2, (lengths.getOrElse(note._2, 0) + 1))
    }

    (lengths.toList.sortBy {entry => entry._2}).last._1
  }

  // Plot a structure with highlight brackets as a sequence of (from start, from end, to start, to end)
  def plotStructure(structure : MelodyStructure, brackets : Seq[(Int, Int, Int, Int)]) : BufferedImage  = {
    // Desired width of modal note length.
    val desiredModalWidth = 10;

    // Height of note.
    var noteHeight = 4

    // todo adaptive to the arcs.
    var topMargin = 300
    var bottomMargin = 10
    var leftMargin = 10
    var rightMargin = 10

    // Spacing between notes.

    val notes = structure.asMonophonic()
    val modalLength = modalDuration(notes)
    val horizontalMultiplier = desiredModalWidth / modalLength;
    val minPitch = notes.minBy(note => note._3)._3
    val maxPitch = notes.maxBy(note => note._3)._3
    val pitchRange =  maxPitch - minPitch

    // Todo width
    val height = (pitchRange * noteHeight).toInt;
    val width = (notes.map{case(offset: Long, duration: Long, pitch:Int) => (duration * horizontalMultiplier)} sum).toInt

    val canvasWidth = width + leftMargin + rightMargin
    val canvasHeight = height + topMargin + bottomMargin

    val buffer  : BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics : Graphics2D  = buffer.createGraphics()


    val hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setRenderingHints(hints)

    // White background.
    graphics.setPaint(Color.white)
    graphics.fillRect(0, 0, canvasWidth, canvasHeight)

    // Draw margins
    // graphics.setPaint(Color.green)
    // graphics.drawRect(1,1,canvasWidth-2,canvasHeight-2)
    // graphics.drawRect(0+topMargin,0+leftMargin,canvasWidth-leftMargin-rightMargin,canvasHeight-topMargin-bottomMargin)

    val pitchX = (pitch : Int) => leftMargin + (pitch.toFloat * horizontalMultiplier).toInt
    val pitchY = (pitch: Int) => topMargin + (height - (pitch - minPitch) * noteHeight.toInt)

    // Draw the notes.
    graphics.setPaint(Color.black)
    for (note <- notes) {
      graphics.fillRect(
        pitchX(note._1.toInt),
        pitchY(note._3),
        (note._2.toFloat * horizontalMultiplier).toInt,
        noteHeight
      )
    }

    // Turn the note indexes to offsets within the tune.
    // Todo: If lookup in notes is O(N) then this could be slow.
    val bracketsWithOnset = brackets.map {case (a: Int, b: Int, c: Int, d: Int) => (notes(a)._1,notes(b)._1 + notes(b)._2,notes(c)._1,notes(d)._1 + notes(d)._2)}
    graphics.setPaint(Color.DARK_GRAY)
    graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));

    for (bracket <- bracketsWithOnset) {
      val firstX = pitchX(bracket._1.toInt)
      val firstWidth = pitchX(bracket._2.toInt) - pitchX(bracket._1.toInt)

      val secondX = pitchX(bracket._3.toInt)
      val secondWidth = pitchX(bracket._4.toInt) - pitchX(bracket._3.toInt)

      // Arc
      var outerRadius =  (secondX + secondWidth) - firstX
      var innerRadius =  secondX - (firstX + firstWidth)

      var path = new Path2D.Double()

      // top outer curve, first to second
      path.moveTo(firstX, topMargin.toInt)
      path.curveTo(firstX, topMargin.toInt,
        (secondX - (firstX + firstWidth)) / 2 + firstX + firstWidth, topMargin.toInt - outerRadius,
        secondX + secondWidth, topMargin.toInt
      )

      // highlight second region
      path.lineTo(secondX + secondWidth, pitchY(minPitch-1))
      path.lineTo(secondX, pitchY(minPitch - 1))
      path.lineTo(secondX, topMargin.toInt)

      // inner curve, second to first
      path.curveTo(secondX, topMargin.toInt,
        (secondX - (firstX + firstWidth)) / 2 + firstX + firstWidth, topMargin.toInt - innerRadius,
        firstX + firstWidth, topMargin.toInt
      )

      // highlight second region
      path.lineTo(firstX + firstWidth, pitchY(minPitch-1))
      path.lineTo(firstX, pitchY(minPitch - 1))
      path.lineTo(firstX, topMargin.toInt)

      graphics.fill(path);
    }

    return buffer
  }
}


// MIDI reading utils.
object Midi {

  // Get the MIDI track with give number.
  def getTrackFromMidiFile(location: String, trackNumber: Integer): Option[Track] = {
    val sequencer = MidiSystem.getSequencer()
    if (sequencer == null) {
      return None;
    } else {
      sequencer.open();
      try {
        val myMidiFile: File = new File(location);
        val mySeq: Sequence = MidiSystem.getSequence(myMidiFile);
        val tracks: Array[Track] = mySeq.getTracks();
        if (tracks.length <= trackNumber) {
          None
        }
        else {
          Some(tracks(trackNumber));
        }
      } catch {
        case e: Exception => None
      }
    }
  }

  def structureFromTrack(track: Track): MelodyStructure = {
    val events = for {eventI <- 0 until track.size} yield track.get(eventI);

    // Pitch bit-mask from midi byte.
    val getPitchByte = (ev: MidiEvent) => ev.getMessage.getMessage()(1)

    val notes = (for (event <- events)
    yield event.getMessage.getStatus match {
        // Note on
        case 0x90 => {
          Some(Tuple3[Symbol, Long, Int]('NoteOn, event.getTick, getPitchByte(event)))
        }
        // Note off
        case 0x80 => {
          Some(Tuple3[Symbol, Long, Int]('NoteOff, event.getTick, getPitchByte(event)))
        }
        case default => None
      }).flatMap(_.toList).toList

    def accumulateTimeDelta(inp: List[Tuple3[Symbol, Long, Int]], time: Long = 0): List[Tuple3[Symbol, Long, Int]] = {
      inp match {
        case x :: xs => Tuple3[Symbol, Long, Int](x._1, x._2, x._3) :: accumulateTimeDelta(xs, time + x._2)
        case Nil => Nil
      }
    }

    return new MelodyStructure(accumulateTimeDelta(notes))
  }
}
}



