/**
 * User: joe
 * Date: 16/02/2013
 * Time: 14:07
 */

package melodysequence {

import javax.sound.midi._
import java.io.File
import scala._
import collection.mutable.ListBuffer
import java.awt.image.BufferedImage
import java.awt.{Color, FontMetrics, Graphics2D, Font}

// TODO tail call!

// A melody structure.
class MelodyStructure(events: List[Tuple3[Symbol, Long, Int]]) {

  // time, duration, offset
  type Note = Tuple3[Long, Long, Int]

  // Convert the list of note on and off durations into a monophonic sequence
  // of (onset, duration, pitch). In the case of polyphony, last one wins!
  def asMonophonic(): Seq[Note] = {

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
  def plotStructure(structure : MelodyStructure) : BufferedImage  = {
    // Todo modal.
    var horizontalMultiplier = 0.0525
    var verticalMultiplier = 1
    var noteHeight = 4

    // Todo width
    val width =  1000;
    val height = 200;

    val buffer  : BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

    val graphics : Graphics2D  = buffer.createGraphics();

    graphics.setPaint(Color.black);

    var notes = structure.asMonophonic()

    // todo unfloat
    for (note <- notes) {
      graphics.fillRect(
        (note._1.toFloat * horizontalMultiplier).toInt,
        height - (note._3 * verticalMultiplier).toInt,
        (note._2.toFloat * horizontalMultiplier).toInt,
        noteHeight
      )
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

    println("notes", notes)

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



