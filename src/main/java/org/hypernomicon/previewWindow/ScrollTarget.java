/*
 * Copyright 2026 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.previewWindow;

import java.util.concurrent.atomic.AtomicLong;

//---------------------------------------------------------------------------

/**
 * Where a preview should scroll once its document and highlights are in
 * place: the clicked-match target carried inside a {@code PreviewIntent}.
 * Because it rides the intent rather than being a fire-and-forget command, a
 * passage click made while the preview window is closed survives the deferral
 * and the window opens scrolled to the clicked match.
 * <p>
 * Paged (pdf.js) content is addressed by {@code pageNum} plus the index of
 * the match among that page's highlight starts; direct content is addressed
 * by the global {@code matchNdx} (highlight spans carry a
 * {@code data-match-ndx} attribute in matches-list order). A target carries
 * all three; the viewer picks by mode.
 * <p>
 * The {@code serial} makes every click a distinct value: the reconciler
 * delivers a target once (so a replayed or re-derived intent never re-fires
 * it against the user), and a repeat click on the same passage arrives as a
 * fresh target that delivers again. Construct via {@link #of}, which assigns
 * the serial.
 */
public record ScrollTarget(int matchNdx, int pageNum, int ndxOnPage, long serial)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final AtomicLong nextSerial = new AtomicLong();

//---------------------------------------------------------------------------

  public static ScrollTarget of(int matchNdx, int pageNum, int ndxOnPage)
  {
    return new ScrollTarget(matchNdx, pageNum, ndxOnPage, nextSerial.incrementAndGet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
