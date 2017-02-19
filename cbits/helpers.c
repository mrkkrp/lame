/*
 * This file is part of ‘lame’ package.
 *
 * Copyright © 2017 Mark Karpov
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name Mark Karpov nor the names of contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "helpers.h"

static unsigned round_to_bytes(unsigned bits)
{
  return (bits + (bits % 8)) / 8;
}

int lame_encoding_helper
  ( lame_global_flags *gfp
  , uint64_t data_offset
  , uint64_t data_size
  , const char * ifile_name
  , const char * ofile_name )
{
  unsigned channels = lame_get_num_channels(gfp);
  unsigned bits_per_sample = 16; /* assuming this for now */
  unsigned msw = round_to_bytes(bits_per_sample);
  unsigned block_align = channels * msw;
  uint64_t samples_to_process = data_size / block_align;
  uint64_t read_size = 4096;
  void *buffer = malloc(read_size * sizeof(short int) * channels);
  void *mp3buffer = malloc(LAME_MAXMP3BUFFER);
  int imp3, status;
  FILE *ifile = fopen(ifile_name, "r");
  FILE *ofile = fopen(ofile_name, "w");
  uint64_t samples;

  if (data_size % block_align)
    {
      free(buffer);
      fclose(ifile);
      fclose(ofile);
      return -1;
    }

  /* move position indicator to beginning of audio data */
  fseek(ifile, data_offset, SEEK_SET);

  while (samples_to_process)
    {
      /* The reading happens by blocks. Every block has read_size samples in
       * it (multi-channel samples, that is). Since we will be using the
       * value returned by fread as indicator of whether something went
       * wrong or not, we need to know beforehand how many samples we need
       * to read. */
      samples = read_size <= samples_to_process ? read_size : samples_to_process;

      /* Read the samples into the “raw” buffer, fail by returning false if
       * number of read samples differs from the expected. */
      if (fread(buffer, block_align, samples, ifile) != samples)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      imp3 = lame_encode_buffer_interleaved
        (gfp, buffer, samples, mp3buffer, LAME_MAXMP3BUFFER);

      if (imp3 < 0)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      status = (int) fwrite(mp3buffer, 1, imp3, ofile);

      if (status != imp3)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      samples_to_process -= samples;
    }

  if (lame_get_nogap_total(gfp))
    imp3 = lame_encode_flush_nogap(gfp, mp3buffer, LAME_MAXMP3BUFFER);
  else
    imp3 = lame_encode_flush(gfp, mp3buffer, LAME_MAXMP3BUFFER);

      if (imp3 < 0)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

  status = (int) fwrite(mp3buffer, 1, imp3, ofile);

  free(buffer);
  fclose(ifile);
  fclose(ofile);

  return (status == imp3) ? 0 : -1;
}
