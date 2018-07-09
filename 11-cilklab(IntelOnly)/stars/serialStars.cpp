#include "serialStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>

ppair serial_friendly_stars(point *inp, int N) {
  float min_dist = std::numeric_limits<float>::infinity();
  ppair closest_pair;

  for (int i=0; i<N; i++) {
    for (int j=i+1; j<N; j++) {
      point p = inp[i];
      point q = inp[j];
      float d = p.dist_squared(q);
      if (d < min_dist) {
        min_dist = d;
        closest_pair = ppair(p, q);
      }
    }
  }
  
  return closest_pair;
}
