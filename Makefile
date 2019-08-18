# to install make on windows:
# https://gist.github.com/evanwill/0207876c3243bbb6863e65ec5dc3f058

EMACS_DIR ?= $(shell pwd)
LOCAL_DIR ?= $(shell if [ -f $(EMACS_DIR)/.local ] ; then echo $$(cat $(EMACS_DIR)/.local) ; else echo $(EMACS_DIR)/local ; fi)
LOCAL_SRC_DIR ?= $(LOCAL_DIR)/src

PIP ?= pip

CMANY_COMPILER ?=

CLANG_VERSION ?= 7.1.0
CLANG_DIR ?= $(LOCAL_SRC_DIR)/clang
CLANG_SRC_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/src
CLANG_BUILD_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/build
CLANG_INSTALL_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/install
CLANG_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CLANG_BUILD_DIR) \
	--install-dir $(CLANG_INSTALL_DIR) \
	$(CLANG_SRC_DIR)

CCLS_REPO ?= https://github.com/MaskRay/ccls
CCLS_BRANCH ?= master  # may also be a tag
CCLS_DIR ?= $(LOCAL_SRC_DIR)/ccls
CCLS_SRC_DIR ?= $(CCLS_DIR)/src
CCLS_BUILD_DIR ?= $(CCLS_DIR)/build
CCLS_INSTALL_DIR ?= $(CCLS_DIR)/install
CCLS_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CCLS_BUILD_DIR) \
	--install-dir $(CCLS_INSTALL_DIR) \
	$(CCLS_SRC_DIR) \
	-V CMAKE_PREFIX_PATH="$(LOCAL_DIR);$(CLANG_BUILD_DIR);$(CLANG_BUILD_DIR)/tools/clang;$(CLANG_SRC_DIR);$(CLANG_SRC_DIR)/tools/clang"


#----------------------------------------------------------------------

# define a function to copy file trees
# usage: $(call copy_tree,src_root,pattern(s),dst_root)
copy_tree = cd $1 && (tar cfp - $2 | (cd $3 ; tar xvf -))

# define a function to make a directory and parents
makedirs = if [ ! -d $1 ] ; then mkdir -p $1 ; fi

#----------------------------------------------------------------------

all: clang_install ccls_install


#----------------------------------------------------------------------

.PHONY: cmany
cmany:
	 # install cmany if needed
	if [ -z "$(shell pip list | grep cmany)" ] ; then \
	    $(PIP) install cmany ; \
	fi


#----------------------------------------------------------------------

.PHONY: ccls ccls_install ccls_build ccls_config ccls_clone
ccls: $(CCLS_INSTALL_DIR)
ccls_build: $(CCLS_INSTALL_DIR)
ccls_config: $(CCLS_BUILD_DIR)
ccls_clone: $(CCLS_SRC_DIR)

.PHONY: clang clang_install clang_build clang_config clang_clone
clang: $(CLANG_INSTALL_DIR)
clang_build: $(CLANG_INSTALL_DIR)
clang_config: $(CLANG_BUILD_DIR)
clang_download: $(CLANG_SRC_DIR)


ccls_install: $(LOCAL_DIR) $(CCLS_INSTALL_DIR)
	@echo "ccls_install: $(CCLS_INSTALL_DIR) ---> $(LOCAL_DIR)"
	@bd=$(shell cmany show_build_names $(CCLS_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	$(call copy_tree,$(CCLS_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))

clang_install: $(LOCAL_DIR) $(CLANG_INSTALL_DIR)
	@echo "clang_install: $(CLANG_INSTALL_DIR) ---> $(LOCAL_DIR)"
	 #cmany show_build_names $(CLANG_CMANY_ARGS)
	bd=$(shell cmany show_build_names $(CLANG_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	echo "Build dir $(CLANG_INSTALL_DIR)/$$bd" ; \
	echo "Local dir $(LOCAL_DIR)" ; \
	(cd $(CLANG_INSTALL_DIR)/$$bd ; ls -l *) ; \
	$(call copy_tree,$(CLANG_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))


$(CCLS_INSTALL_DIR): $(CCLS_BUILD_DIR)
	@echo "ccls_install_dir: $(CCLS_INSTALL_DIR)"
	cmany i $(CCLS_CMANY_ARGS)

$(CLANG_INSTALL_DIR): $(CLANG_BUILD_DIR)
	@echo "clang_install_dir: $(CLANG_INSTALL_DIR)"
	cmany b $(CLANG_CMANY_ARGS)


$(CCLS_BUILD_DIR): cmany $(CCLS_SRC_DIR)
	@echo "ccls_build_dir: $(CCLS_INSTALL_DIR)"
	cmany c $(CCLS_CMANY_ARGS)

$(CLANG_BUILD_DIR): cmany $(CLANG_SRC_DIR)
	@echo "clang_build_dir: $(CLANG_INSTALL_DIR)"
	cmany c $(CLANG_CMANY_ARGS)


$(CCLS_SRC_DIR): $(CCLS_DIR)
	@echo "ccls_src_dir: $(CCLS_SRC_DIR)"
	if [ ! -d "$(CCLS_SRC_DIR)" ] ; then \
	    cd $(LOCAL_SRC_DIR) && git clone --recursive --branch=$(CCLS_BRANCH) $(CCLS_REPO) $(CCLS_SRC_DIR) ; \
	fi

$(CLANG_SRC_DIR): $(CLANG_DIR)
	@echo "clang_src_dir: $(CLANG_SRC_DIR)"
	if [ ! -d "$(CLANG_SRC_DIR)" ] ; then \
	    git clone --recursive https://github.com/biojppm/clang-build $(CLANG_SRC_DIR) ; \
	fi
	@#if [ ! -d "$(CLANG_SRC_DIR)" ] ; then \
	 #    cd $(LOCAL_SRC_DIR) && git clone --recursive --branch=$(CLANG_BRANCH) $(CLANG_REPO) $(CLANG_SRC_DIR) ; \
	 #fi
	@#if [ ! -d "$(CLANG_SRC_DIR)" ] ; then \
	 #    git clone --recursive --branch=$(CLANG_BRANCH) https://git.llvm.org/git/llvm.git $(CLANG_SRC_DIR) ; \
	 #fi
	@#if [ ! -d "$(CLANG_SRC_DIR)/tools/clang" ] ; then \
	 #    git clone --recursive --branch=$(CLANG_BRANCH) https://git.llvm.org/git/clang.git $(CLANG_SRC_DIR)/tools/clang ; \
	 #fi

$(CCLS_DIR):
	$(call makedirs, $(CCLS_DIR))

$(CLANG_DIR):
	$(call makedirs, $(CLANG_DIR))

$(LOCAL_SRC_DIR):
	$(call makedirs, $(LOCAL_SRC_DIR))

$(LOCAL_DIR):
	$(call makedirs, $(LOCAL_DIR))
