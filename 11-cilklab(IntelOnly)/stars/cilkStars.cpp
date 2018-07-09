#include "cilkStars.h"
#include "serialStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>
#include <algorithm>

int compareX (const void* a, const void* b) {
  point* p = (point*)a;
  point* q = (point*)b;
  return p->x - q->x;
}

int compareY (const void* a, const void* b) {
  point* p = (point*)a;
  point* q = (point*)b;
  return p->y - q->y;
}

ppair stripClosest(point *strip, int n, ppair closest_pair, float min_dist) {
  for (int i = 0; i<n; i++) {
    point p = strip[i];
    // This loop is guaranteed to run at most 6 times
    for (int j=i+1; j<n && (strip[j].y - p.y)<std::sqrt(min_dist); ++j) {
      point q = strip[j];
      float d = p.dist_squared(q);
      if (d < min_dist) {
        min_dist = d;
        closest_pair = ppair(p, q);
      }
    }
  }
  
  return closest_pair;
}

#define SERIAL_THRESHOLD 500

ppair cilk_friendly_stars_helper(point *inpX, point *inpY, int N) {
  if (N <= SERIAL_THRESHOLD)
    return serial_friendly_stars(inpX, N);

  double midX = inpX[N/2].x;
  
  point *yL = new point[N];
  point *yR = new point[N];
  int li = 0, ri = 0;
  for (int i=0; i < N; i++) {
    point p = inpY[i];
    if (p.x <= midX)
       yL[li++] = p;
    else
       yR[ri++] = p;
  }
  
  if (li < 2 || ri < 2)
    return serial_friendly_stars(inpX, N);

  ppair pL = cilk_spawn cilk_friendly_stars_helper(inpX, yL, li);
  ppair pR = cilk_friendly_stars_helper(inpX + li, yR, ri);
  cilk_sync;

  float pL_dist = pL.p1.dist_squared(pL.p2);
  float pR_dist = pR.p1.dist_squared(pR.p2);
  ppair closest_pair = (pL_dist <= pR_dist) ? pL : pR;
  float min_dist = closest_pair.p1.dist_squared(closest_pair.p2);

  point *strip = new point[N];
  int j = 0;
  for (int i=0; i < N; i++) {
    point p = inpY[i];
    if (abs(p.x - midX) < min_dist) {
      strip[j++] = p;
    }
  }

  return stripClosest(strip, j, closest_pair, min_dist);
}

ppair cilk_friendly_stars(point *inp, int N) {
  point *inpX = new point[N];
  point *inpY = new point[N];
  for (int i=0; i<N; i++) {
      inpX[i] = inp[i];
      inpY[i] = inp[i];
  }
  
  cilk_spawn qsort(inpX, N, sizeof(point), compareX);
  qsort(inpY, N, sizeof(point), compareY);
  cilk_sync;
  
  return cilk_friendly_stars_helper(inpX, inpY, N);
}
