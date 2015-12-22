package uk.ac.cam.bsc28.diss.CodeGeneration

import java.util.UUID

object LabelGenerator {

  def nextLabel(): String = {
    UUID.randomUUID().toString
  }

}
