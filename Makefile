# to install make on windows:
# https://gist.github.com/evanwill/0207876c3243bbb6863e65ec5dc3f058

EMACS_DIR ?= $(shell pwd)
UTIL_DIR ?= $(EMACS_DIR)/util
LOCAL_DIR ?= $(EMACS_DIR)/util/local

PIP ?= pip

CMANY_COMPILER ?= -c vs2017

CLANG_REPO ?= https://github.com/llvm/llvm-project
CLANG_BRANCH ?= llvmorg-7.0.1  # may also be a tag
CLANG_DIR ?= $(UTIL_DIR)/llvm
CLANG_SRC_DIR ?= $(CLANG_DIR)/$(CLANG_BRANCH)/src
CLANG_BUILD_DIR ?= $(CLANG_DIR)/$(CLANG_BRANCH)/build
CLANG_INSTALL_DIR ?= $(CLANG_DIR)/$(CLANG_BRANCH)/install
CLANG_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CLANG_BUILD_DIR) \
	--install-dir $(CLANG_INSTALL_DIR) \
	-V LLVM_TOOL_LLD_BUILD=ON \
	-V LLVM_TOOL_LLDB_BUILD=ON \
	-V LLVM_TOOL_CLANG_BUILD=ON \
	$(CLANG_SRC_DIR)/llvm

CCLS_REPO ?= https://github.com/MaskRay/ccls
CCLS_BRANCH ?= master  # may also be a tag
CCLS_DIR ?= $(UTIL_DIR)/ccls
CCLS_SRC_DIR ?= $(CCLS_DIR)/src
CCLS_BUILD_DIR ?= $(CCLS_DIR)/build
CCLS_INSTALL_DIR ?= $(CCLS_DIR)/install
CCLS_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CCLS_BUILD_DIR) \
	--install-dir $(CCLS_INSTALL_DIR) \
	$(CCLS_SRC_DIR) \
	-V CMAKE_CXX_COMPILER=clang-cl \
	-V CMAKE_PREFIX_PATH="$(LOCAL_DIR);$(CLANG_BUILD_DIR);$(CLANG_BUILD_DIR)/tools/clang;$(CLANG_SRC_DIR);$(CLANG_SRC_DIR)/tools/clang"


#----------------------------------------------------------------------

# define a function to copy file trees
# usage: $(call copy_tree,dst,src,pattern(s))
copy_tree = cd $1 && (tar cfp - $2 | (cd $3 ; tar xvf -))

#----------------------------------------------------------------------

all: ccls_install clang_install


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
clang_clone: $(CLANG_SRC_DIR)


ccls_install: $(LOCAL_DIR) #$(CCLS_INSTALL_DIR)
	@bd=$(shell cmany show_build_names $(CCLS_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	$(call copy_tree,$(CCLS_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))

clang_install: $(LOCAL_DIR) #$(CLANG_INSTALL_DIR)
	@bd=$(shell cmany show_build_names $(CLANG_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	$(call copy_tree,$(CLANG_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))


$(CCLS_INSTALL_DIR): $(CCLS_BUILD_DIR)
	cmany i $(CCLS_CMANY_ARGS)

$(CLANG_INSTALL_DIR): $(CLANG_BUILD_DIR)
	cmany i $(CLANG_CMANY_ARGS)


$(CCLS_BUILD_DIR): cmany $(CCLS_SRC_DIR)
	cmany c $(CCLS_CMANY_ARGS)

$(CLANG_BUILD_DIR): cmany $(CLANG_SRC_DIR)
	cmany c $(CLANG_CMANY_ARGS)


$(CCLS_SRC_DIR): $(CCLS_DIR)
	if [ ! -d "$(CCLS_SRC_DIR)" ] ; then \
	    cd $(UTIL_DIR) && git clone --recursive --branch=$(CCLS_BRANCH) $(CCLS_REPO) $(CCLS_SRC_DIR) ; \
	fi

$(CLANG_SRC_DIR): $(CLANG_DIR)
	@#cd $(UTIL_DIR) && git clone --recursive --branch=$(CLANG_BRANCH) https://git.llvm.org/git/llvm.git
	@#cd $(UTIL_DIR) && git clone --recursive --branch=$(CLANG_BRANCH) https://git.llvm.org/git/clang.git llvm/tools/clang
	if [ ! -d "$(CLANG_SRC_DIR)" ] ; then \
	    cd $(UTIL_DIR) && git clone --recursive --branch=$(CLANG_BRANCH) $(CLANG_REPO) $(CLANG_SRC_DIR) ; \
	fi

$(CCLS_DIR):
	if [ ! -d $(CCLS_DIR) ] ; then mkdir -p $(CCLS_DIR) ; fi

$(CLANG_DIR):
	if [ ! -d $(CLANG_DIR) ] ; then mkdir -p $(CLANG_DIR) ; fi

$(UTIL_DIR):
	if [ ! -d $(UTIL_DIR) ] ; then mkdir -p $(UTIL_DIR) ; fi

$(LOCAL_DIR):
	if [ ! -d $(LOCAL_DIR) ] ; then mkdir -p $(LOCAL_DIR) ; fi
