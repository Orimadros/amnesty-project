// deforestation_rules.cpp
// Migrated verbatim from legacy_repo/code/0_deforestation_rules.cpp (logic unchanged).
//
// Pure string-logic kernel: takes a CharacterMatrix where each row is a pixel and
// each column is a year's land-cover label ("forest" / "human" / "deforested" /
// "reforested" / ""), and applies the deforestation/reforestation persistence rules
// left-to-right across the years. Touches no spatial libraries, so its output is
// identical on host R or inside the container.

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterMatrix apply_deforestation_rules_cpp(CharacterMatrix df) {
  int nRow = df.nrow();
  int nCol = df.ncol();

  for (int i = 0; i < nRow; ++i) {
    for (int j = 0; j < nCol - 2; ++j) {
      // Apply Rule 1
      if ((df(i, j) == "forest" && df(i, j + 1) == "forest" && df(i, j + 2) == "human") ||
          (df(i, j + 1) == "deforested" && df(i, j + 2) == "human")) {
        df(i, j + 2) = "deforested";
      }
    }

    for (int j = 0; j < nCol - 4; ++j) {
      // Apply Rule 2
      if ((df(i, j) == "deforested" && df(i, j + 1) == "deforested" &&
           df(i, j + 2) == "forest" && df(i, j + 3) == "forest" && df(i, j + 4) == "forest") ||
          (df(i, j + 3) == "reforested" && df(i, j + 4) == "forest")) {
        df(i, j + 4) = "reforested";
      }
    }

    for (int j = 0; j < nCol - 2; ++j) {
      // Additional Rule
      if ((df(i, j) == "reforested" && df(i, j + 1) == "reforested" && df(i, j + 2) == "human") ||
          (df(i, j + 1) == "deforested" && df(i, j + 2) == "human")) {
        df(i, j + 2) = "deforested";
      }
    }
  }

  return df;
}
