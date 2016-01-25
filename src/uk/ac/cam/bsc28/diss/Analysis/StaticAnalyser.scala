package uk.ac.cam.bsc28.diss.Analysis

import uk.ac.cam.bsc28.diss.FrontEnd.ParseTree

object StaticAnalyser {

  type AnalysisError = String
  type Analysis = ParseTree.Node => Option[AnalysisError]

  private val analyses : List[Analysis] = List(LetAnalyser.analyse)

  def analyse(tree: ParseTree.Node): Option[List[AnalysisError]] = {
    val errors = analyses map (f => f(tree))
    if (errors.forall(opt => opt.isEmpty)) {
      None
    } else {
      Some(errors.filter(opt => opt.nonEmpty).map(err => err.get))
    }
  }

}
