# Makefile --- GNU Makefile to build Guile code

# Copyright © 2026 Alex Kost <alezost@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# The only purpose of this Makefile is to build "init.scm" to check it
# for potential errors.

SCM_FILES = init.scm
GO_FILES = $(SCM_FILES:%.scm=%.go)

GUILEC_ENV =					\
  GUILE_AUTO_COMPILE=0

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

GUILEC_ENV +=								\
  GUILE_WARN_DEPRECATED=detailed

all: $(GO_FILES)

$(GO_FILES): %.go: %.scm
	@$(GUILEC_ENV) guild compile $(GUILEC_OPTS) --output=$@ $<

clean:
	$(RM) -f $(GO_FILES)

.PHONY: clean scripts

# Makefile ends here
