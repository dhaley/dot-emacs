# pumpio-el

Pumpio Client for Emacs

# Requirements

* [emacs-OAuth](https://github.com/psanford/emacs-oauth) 
  * Rename or delete hmac-sha1.el from this proyect.
* [hmac-sha1 from FLIM](https://github.com/wanderlust/flim) (Keep reading for explanations).
* URL package. Comes by default with Emacs 24.
* JSON package. Comes by default with Emacs 24.

## Considerations With the OAuth Library

OAuth protocol requires a hash for signatures and usually use HMAC-SHA1. Despite emacs-OAuth comes with its own hmac-sha1.el implementation, for some reason *doesn't work and you have to download another*.

I used the FLIM implementation and works well. Until I solve this problem, download the FLIM library completely at the [wanderlust github group](https://github.com/wanderlust/flim) and install it.

Please, erase or rename the hmac-sha1.el file that comes with emacs-OAuth, it won't work for connecting with pump.io.

# License


    README.md
    Copyright (C) 2013  Giménez, Christian N.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Viernes 28 De Junio Del 2013


