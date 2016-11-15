/* =============================================================================
 * quickblock -- Blocking in Experiments with Large Samples
 * https://github.com/fsavje/quickblock
 *
 * Copyright (C) 2016  Fredrik Savje -- http://fredriksavje.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/
 * ========================================================================== */

#ifndef QBC_RERROR_HG
#define QBC_RERROR_HG

#define qbc_Rerror(msg) (qbc_Rerror__(msg, __FILE__, __LINE__))

void qbc_Rerror__(const char* msg,
                  const char* file,
                  int line);

#endif // ifndef QBC_RERROR_HG

