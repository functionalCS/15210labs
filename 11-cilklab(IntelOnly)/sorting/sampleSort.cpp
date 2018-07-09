// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2010 Guy Blelloch and Harsha Vardhan Simhadri and the PBBS team
//

#include "quickSort.h"
#include <stdlib.h>
#include <iostream>
#include <math.h>
//#include "CycleTimer.h"

int search(double* S, int n, double v) {
  for (int i=0; i < n; i++)
    if (v < S[i]) return i;
  return n;
}

int binSearch(double* S, int n, double v) {
  double* T = S;
  while (n > 10) {
    int mid = n/2;
    if (v < T[mid]) n = mid;
    else {
      n = (n-mid)-1;
      T = T + mid + 1;
    }
  }
  return T-S+search(T,n,v);
}

void genCounts(int *Index, int *counts, int m, int n) {
  for (int i = 0; i < m; i++)
    counts[i] = 0;
  for (int j = 0; j < n; j++)
    counts[Index[j]]++;
}

void relocate(double* A, double* B, int *Index, int *offsets, int n) {
  for (long j = 0; j < n; j++)
    B[offsets[Index[j]]++] = A[j];
}

#define SSORT_THR 1

void sampleSortSerial (double* A, int n) {
  if (n < SSORT_THR) quickSortSerial(A, n);
  else {
    int isqrt = (int) sqrt((double) n);
    int numBlocks = isqrt/10;
    int numBuckets = isqrt/10;
    int blockSize = (n-1)/numBlocks + 1;
    double* pivots = new double[numBuckets-1];

    // generate numBuckets-1 random pivots
    for (int j=0; j< numBuckets-1; ++j)
      pivots[j] = A[rand()%n];

    // sort the pivots
    quickSortSerial(pivots, numBuckets-1);

    // determine for each key which bucket it goes into
    int* bucketId = new int[n];
    for(int i = 0; i < n; i++)
      bucketId[i] = binSearch(pivots,numBuckets-1,A[i]);
    delete[] pivots;

    // count size of bucket for each of the blocks
    int* counts = new int[numBlocks*numBuckets];
    for (int i2 = 0; i2 < numBlocks; i2++) {
      int offset = i2 * blockSize;
      int size =  (i2 < numBlocks - 1) ? blockSize : n - offset;
      genCounts(bucketId + offset, counts + i2 * numBuckets, numBuckets, size);
    }

    // generate offsets
    int sum = 0;
    for (int i = 0; i < numBuckets; i++)
      for (int j = 0; j < numBlocks; j++) {
      	int v = counts[j*numBuckets+i];
      	counts[j*numBuckets+i] = sum;
      	sum += v;
      }

    int* bucketStarts = new int[numBuckets+1];
    for (int i = 0; i < numBuckets; i++)
      bucketStarts[i] = counts[i];
    bucketStarts[numBuckets] = n;

    double* B = new double[n];
    for (int i3 = 0; i3 < numBlocks; i3++) {
      int offset = i3 * blockSize;
      int size =  (i3 < numBlocks - 1) ? blockSize : n - offset;
      relocate(A + offset, B, bucketId + offset,
	             counts + i3 * numBuckets, size);
    }

    // Sort each bucket
    for (int r = 0; r < numBuckets; r++) {
      int size = bucketStarts[r+1] - bucketStarts[r];
      for (int i=bucketStarts[r]; i < bucketStarts[r+1]; i++)
        A[i] = B[i];
      quickSortSerial(A + bucketStarts[r], size);
    }

    delete[] counts;
    delete[] bucketId;
    delete[] B;
    delete[] bucketStarts;
  }
}

void sampleSort (double* A, int n) {
  if (n < SSORT_THR) quickSortSerial(A, n);
  else {
    int isqrt = (int) sqrt((double) n);
    int numBlocks = isqrt/10;
    int numBuckets = isqrt/10;
    int blockSize = (n-1)/numBlocks + 1;
    double* pivots = new double[numBuckets-1];

    // generate numBuckets-1 random pivots
    for (int j=0; j< numBuckets-1; ++j)
      pivots[j] = A[rand()%n];

    // sort the pivots
    quickSortSerial(pivots, numBuckets-1);

    // determine for each key which bucket it goes into
    int* bucketId = new int[n];
    cilk_for(int i = 0; i < n; i++)
      bucketId[i] = binSearch(pivots,numBuckets-1,A[i]);
    delete[] pivots;

    int* counts = new int[numBlocks*numBuckets];
    cilk_for (int i2 = 0; i2 < numBlocks; i2++) {
      int offset = i2 * blockSize;
      int size =  (i2 < numBlocks - 1) ? blockSize : n - offset;
      genCounts(bucketId + offset, counts + i2 * numBuckets, numBuckets, size);
    }

    // generate offsets
    int* bucketStarts = new int[numBuckets+1];
    cilk_for (int i = 0; i < numBuckets; i++) {
      int sum = 0;
      for (int j = 0; j < numBlocks; j++)
      	sum += counts[j*numBuckets+i];
      bucketStarts[i] = sum;
    }
    int sum = 0;
    for (int i = 0; i < numBuckets; i++) {
      int v = bucketStarts[i];
      bucketStarts[i] = sum;
      sum += v;
    }
    cilk_for (int i = 0; i < numBuckets; i++) {
      int sum = bucketStarts[i];
      for (int j = 0; j < numBlocks; j++) {
      	int v = counts[j*numBuckets+i];
      	counts[j*numBuckets+i] = sum;
      	sum += v;
      }
    }
    bucketStarts[numBuckets] = n;

    double* B = new double[n];
    cilk_for (int i3 = 0; i3 < numBlocks; i3++) {
      int offset = i3 * blockSize;
      int size =  (i3 < numBlocks - 1) ? blockSize : n - offset;
      relocate(A + offset, B, bucketId + offset,
         counts + i3 * numBuckets,
         size);
    }

    // Sort each bucket
    cilk_for (int r = 0; r < numBuckets; r++) {
      int size = bucketStarts[r+1] - bucketStarts[r];
      for (int i=bucketStarts[r]; i < bucketStarts[r+1]; i++)
        A[i] = B[i];
      quickSortSerial(A + bucketStarts[r], size);
    }

    delete[] counts;
    delete[] bucketId;
    delete[] B;
    delete[] bucketStarts;
  }
}
