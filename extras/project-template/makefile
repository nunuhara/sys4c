.SUFFIXES:
.PHONY: clean

srcdir = src
libdir = lib
objdir = obj
outdir = out

# The output file name
target = $(outdir)/out.ain

# List of native libraries supported by the target engine version.
# There should be corresponding .hll files in lib/ for each library.
# This list is based on the engine version shipped with Sengoku Rance.
libs = MsgLogManager MsgLogViewer SACT2 DrawGraph DrawPluginManager Math \
       MsgSkip SystemService OutputLog MusicSystem AliceLogo2 PlayMovie \
       Confirm2 File InputString

# List of compilation units (files in $(srcdir))
# Any new source files should be added to this list
units = foo object main

# The dependencies for each unit are specified manually below
# There should be one line for each source file
foo_deps    = object
main_deps   = foo object
object_deps =

#
# The definitions below need not be changed.
#

# Macros mapping unit names to file names
unitfiles = $(addprefix $(objdir)/,$(addsuffix .ain,$(1)))
srcfiles = $(addprefix $(srcdir)/,$(addsuffix .jaf,$(1)))
libfiles = $(addprefix $(libdir)/,$(addsuffix .hll,$(1)))
lib_unit = $(objdir)/_lib.ain
all_units = $(lib_unit) $(call unitfiles,$(units))

all: $(target)

$(objdir)/%.ain: $(srcdir)/%.jaf
	@./build-unit.sh $@ $(lib_unit) $< $(call unitfiles,$($*_deps))

$(objdir)/_lib.ain: $(call libfiles,$(libs))
	sys4c -c -o $@ $^

# Macro to add dependencies as prerequisites for a given unit
define unit_dep_template =
$(call unitfiles,$(1)): $(call srcfiles,$(1)) $(lib_unit) $(call unitfiles,$($(1)_deps))
endef

# Add prerequisites
$(foreach f,$(units),$(eval $(call unit_dep_template,$(f))))

$(target): $(all_units)
	sys4c -o $@ $^

clean:
	rm -f $(all_units) $(target)
