# to install make on windows:
# https://gist.github.com/evanwill/0207876c3243bbb6863e65ec5dc3f058

EMACS_DIR ?= $(shell pwd)
LOCAL_DIR ?= $(shell if [ -f $(EMACS_DIR)/.local ] ; then echo $$(cat $(EMACS_DIR)/.local) ; else echo $(EMACS_DIR)/local ; fi)
LOCAL_SRC_DIR ?= $(LOCAL_DIR)/src
PIP ?= pip

BAZEL_VERSION = 3.6.0
BAZEL_VERSION_URL = "https://github.com/bazelbuild/bazel/releases/download/$(BAZEL_VERSION)/bazel-$(BAZEL_VERSION)-windows-x86_64.exe"
RIPGREP_VERSION = 12.0.1
RIPGREP_VERSION_URL = "https://github.com/BurntSushi/ripgrep/releases/download/$(RIPGREP_VERSION)/ripgrep-$(RIPGREP_VERSION)-i686-pc-windows-msvc.zip"
AG_VERSION_URL = "https://github.com/k-takata/the_silver_searcher-win32/releases/download/2019-03-23%2F2.2.0-19-g965f71d/ag-2019-03-23_2.2.0-19-g965f71d-x64.zip"
FZF_VERSION_URL = "https://github.com/junegunn/fzf-bin/releases/download/0.21.1/fzf-0.21.1-windows_amd64.zip"
FD_VERSION_URL = "https://github.com/sharkdp/fd/releases/download/v7.5.0/fd-v7.5.0-i686-pc-windows-msvc.zip"
PANDOC_VERSION = 2.9.2
PANDOC_VERSION_URL = "https://github.com/jgm/pandoc/releases/download/$(PANDOC_VERSION)/pandoc-$(PANDOC_VERSION)-windows-x86_64.zip"
IMAGE_MAGICK_URL = "https://imagemagick.org/download/binaries/ImageMagick-7.0.10-28-portable-Q16-x64.zip"
MARKDOWN_TOC = https://raw.githubusercontent.com/ekalinin/github-markdown-toc/master/gh-md-toc
TCPVIEW_URL = https://download.sysinternals.com/files/TCPView.zip
MARP_ZIP = "https://github.com/marp-team/marp-cli/releases/download/v4.0.2/marp-cli-v4.0.2-win.zip"
MARP_TGZ = "https://github.com/marp-team/marp-cli/releases/download/v4.0.2/marp-cli-v4.0.2-linux.tar.gz"
IPERF_ZIP = "https://iperf.fr/download/windows/iperf-3.1.3-win64.zip"
SWIG_ZIP = "http://prdownloads.sourceforge.net/swig/swigwin-4.1.1.zip"
IRFANVIEW_ZIP = "https://www.irfanview.info/files/iview457_x64.zip"
DEPENDS22_ZIP = "https://www.dependencywalker.com/depends22_x64.zip"

GCC14 = https://github.com/xpack-dev-tools/gcc-xpack/releases/download/v14.2.0-2/xpack-gcc-14.2.0-2-linux-x64.tar.gz


CMANY_COMPILER ?=

CLANG_VERSION ?= 11.0.0
CLANG_DIR ?= $(LOCAL_SRC_DIR)/clang
CLANG_SRC_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/src
CLANG_BUILD_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/build
CLANG_INSTALL_DIR ?= $(CLANG_DIR)/$(CLANG_VERSION)/install
CLANG_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CLANG_BUILD_DIR) \
	--install-dir $(CLANG_INSTALL_DIR) \
        -V CLANG_VERSION=$(CLANG_VERSION) \
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
	-V CMAKE_PREFIX_PATH="$(LOCAL_DIR);$(CLANG_BUILD_DIR);$(CLANG_BUILD_DIR)/tools/clang;$(CLANG_SRC_DIR);$(CLANG_SRC_DIR)/tools/clang" \
	$(CCLS_SRC_DIR)


RTAGS_REPO ?= https://github.com/Andersbakken/rtags
RTAGS_BRANCH ?= master  # may also be a tag
RTAGS_DIR ?= $(LOCAL_SRC_DIR)/rtags
RTAGS_SRC_DIR ?= $(RTAGS_DIR)/src
RTAGS_BUILD_DIR ?= $(RTAGS_DIR)/build
RTAGS_INSTALL_DIR ?= $(RTAGS_DIR)/install
RTAGS_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(RTAGS_BUILD_DIR) \
	--install-dir $(RTAGS_INSTALL_DIR) \
	-V CMAKE_PREFIX_PATH="$(LOCAL_DIR);$(CLANG_BUILD_DIR);$(CLANG_BUILD_DIR)/tools/clang;$(CLANG_SRC_DIR);$(CLANG_SRC_DIR)/tools/clang" \
	$(RTAGS_SRC_DIR)

CQUERY_REPO ?= https://github.com/cquery-project/cquery
CQUERY_BRANCH ?= master  # may also be a tag
CQUERY_DIR ?= $(LOCAL_SRC_DIR)/cquery
CQUERY_SRC_DIR ?= $(CQUERY_DIR)/src
CQUERY_BUILD_DIR ?= $(CQUERY_DIR)/build
CQUERY_INSTALL_DIR ?= $(CQUERY_DIR)/install
CQUERY_CMANY_ARGS ?= $(CMANY_COMPILER) \
	--build-dir $(CQUERY_BUILD_DIR) \
	--install-dir $(CQUERY_INSTALL_DIR) \
	-V SYSTEM_CLANG=ON \
	-V CMAKE_PREFIX_PATH="$(LOCAL_DIR);$(CLANG_BUILD_DIR);$(CLANG_BUILD_DIR)/tools/clang;$(CLANG_SRC_DIR);$(CLANG_SRC_DIR)/tools/clang" \
	$(CQUERY_SRC_DIR)

# https://stackoverflow.com/questions/714100/os-detecting-makefile
ifeq ($(OS),Windows_NT)
    OS := Windows
    DL_DIR = "$(SYSTEMDRIVE)/Users/$(USERNAME)/Downloads"
else
    UNAME_S := $(shell uname -s)
    DL_DIR := $(HOME)/tmp/download
    # https://askubuntu.com/questions/279168/detect-if-its-ubuntu-linux-os-in-makefile
    ifeq ($(UNAME_S),Linux)
        OS := Linux
	ifeq ($(shell ls /etc/arch-release),/etc/arch-release)
	    DISTRO := Arch
	else
	    # Ubuntu?
	    DISTRO := $(shell lsb_release -si | sed 's/Linux//' | sed 's/[[:blank:]]//g')
	endif
    else
        ifeq ($(UNAME_S),Darwin)
            OS := Darwin
        endif
    endif
endif


#----------------------------------------------------------------------

# define a function to copy file trees
# usage: $(call copy_tree,src_root,pattern(s),dst_root)
copy_tree = set -xe ; cd $1 && (tar cfp - $2 | (cd $3 ; tar xvf -))

# define a function to make a directory and parents
makedirs = if [ ! -d $1 ] ; then mkdir -p $1 ; fi

# download a url $1 to a destination file $2
download = curl -o $2 -L -s "$1"

# install a pip package
pipinstall = set -x ; if [ -z "$(shell pip list | grep $1)" ] ; then $(PIP) install $1 ; fi

# download and unpack a windows zip
# $1=url
# $2=unpack pattern
wininstallzip = \
	set -xe ; \
	mkdir -p $(DL_DIR) ; \
	fn=`basename $1 | sed 's:\.zip$$::g'` ; \
	curl -o $(DL_DIR)/$$fn.zip -L -s "$1" ; \
	7z x $(DL_DIR)/$$fn.zip -y -o$(DL_DIR)/$$fn ; \
        ( cd $(DL_DIR)/$$fn && cp -favr $2 $(LOCAL_DIR)/bin/ )

# download and unpack a tgz
# $1=url
# $2=extension (eg tgz or tar.gz)
# $3=unpack pattern
linuxinstalltgz = \
	set -xe ; \
	mkdir -p $(DL_DIR) ; \
	fn=`basename $1 | sed "s:\.$2$$::g"` ; \
	curl -o $(DL_DIR)/$$fn.$2 -L -s "$1" ; \
	mkdir -p $(DL_DIR)/$$fn.extract ; \
	( \
	  cd $(DL_DIR)/$$fn.extract ; \
	  tar xvfz $(DL_DIR)/$$fn.$2 ; \
	  cp -favr $3 $(LOCAL_DIR)/bin/ \
	) ; \
	rm -rf $(DL_DIR)/$$fn.extract


#----------------------------------------------------------------------

all: ripgrep \
	ag \
	fzf \
	fd \
	pandoc \
	image_magick \
	markdown_toc \
	cmany \
	pip_packages \
	clang_install \
	system_only \
	marp \
	swig

ifeq ($(OS),Windows_NT)
system_only: windows_only
else
system_only: linux_only
endif
windows_only: tcpview iperf irfanview depends22_x86 depends22
linux_only:


#----------------------------------------------------------------------

.PHONY: cmany
cmany:
	$(call pipinstall, cmany)


.PHONY: bazel
bazel: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call download,$(BAZEL_VERSION_URL),$(LOCAL_DIR)/bin/bazel.exe) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S bazel ; \
	   else \
	      sudo apt-get install bazel ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: pip_packages
pip_packages:
	$(call pipinstall, pillow)
	$(call pipinstall, grip)


.PHONY: ripgrep
ripgrep: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(RIPGREP_VERSION_URL),*.*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S ripgrep ; \
	   else \
	      sudo add-apt-repository ppa:x4121/ripgrep ; \
	      sudo apt-get update ; \
	      sudo apt-get install ripgrep ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: ag
ag: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(AG_VERSION_URL),ag.exe) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S the_silver_searcher ; \
	   else \
	      sudo add-apt-repository ppa:pgolm/the-silver-searcher ; \
	      sudo apt-get update ; \
	      sudo apt-get install the-silver-searcher ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


# https://github.com/junegunn/fzf
.PHONY: fzf
fzf: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(FZF_VERSION_URL),*.*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S fzf ; \
	   else \
              git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf ; \
	      ~/.fzf/install ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


# https://github.com/sharkdp/fd/
.PHONY: fd
fd: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(FD_VERSION_URL),*.*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S fd ; \
	   else \
	      bbbbbbbb not done ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: pandoc
pandoc: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(PANDOC_VERSION_URL),pandoc-$(PANDOC_VERSION)/*.exe) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S pandoc ; \
	   else \
	      sudo apt-get install pandoc ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: image_magick
image_magick: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(IMAGE_MAGICK_URL),*.exe *.dll) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      sudo pacman -S imagemagick ; \
	   else \
	      sudo apt-get install imagemagick ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: markdown_toc
markdown_toc: $(LOCAL_DIR)/bin
	$(call download,$(MARKDOWN_TOC),$(LOCAL_DIR)/bin/gh-md-toc)
	chmod a+x $(LOCAL_DIR)/bin/gh-md-toc


.PHONY: marp
marp: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(MARP_ZIP),*.exe) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      yay -S marp-cli ; \
	   else \
	      $(call linuxinstalltgz,$(MARP_TGZ),tgz,marp) ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: iperf
iperf: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(IPERF_ZIP),iperf*/*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   bbbbbbbb not done ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: swig
swig: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(SWIG_ZIP),swig*/*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      pacman -S swig ; \
	   else \
	      apt-get install -Y swig ; \
	   fi ; \
	fi


.PHONY: irfanview
irfanview: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(IPERF_ZIP),iperf*/*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   bbbbbbbb not done ; \
	else \
	   bbbbbbbb not done ; \
	fi

.PHONY: depends22
depends22: $(LOCAL_DIR)/bin
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   $(call wininstallzip,$(DEPENDS22_ZIP),*) ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   bbbbbbbb not done ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: dropbox
dropbox:
	 # https://forum.manjaro.org/t/cannot-upgrade-dropbox-key-issue/71432
	set -x ; set -e ; \
	if [ "$(OS)" == "Windows" ] ; then \
	   aaaaaaaa not done ; \
	elif [ "$(OS)" == "Linux" ] ; then \
	   if [ "$(DISTRO)" == "Manjaro" ] || [ "$(DISTRO)" == "Arch" ] ; then \
	      gpg --recv-key --keyserver hkp://pgp.mit.edu FC918B335044912E ; \
	      trizen -S dropbox dropbox-cli ; \
	   else \
	      sudo apt-get install dropbox ; \
	   fi ; \
	else \
	   bbbbbbbb not done ; \
	fi


.PHONY: tcpview
tcpview:
	$(call wininstallzip,$(TCPVIEW_URL),*.exe *.chm *.HLP)


#----------------------------------------------------------------------

.PHONY: rtags rtags_install rtags_build rtags_config rtags_clone
rtags: $(RTAGS_INSTALL_DIR)
rtags_build: $(RTAGS_INSTALL_DIR)
rtags_config: $(RTAGS_BUILD_DIR)
rtags_clone: $(RTAGS_SRC_DIR)

.PHONY: cquery cquery_install cquery_build cquery_config cquery_clone
cquery: $(CQUERY_INSTALL_DIR)
cquery_build: $(CQUERY_INSTALL_DIR)
cquery_config: $(CQUERY_BUILD_DIR)
cquery_clone: $(CQUERY_SRC_DIR)

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


rtags_install: $(LOCAL_DIR) $(RTAGS_INSTALL_DIR)
	@echo "rtags_install: $(RTAGS_INSTALL_DIR) ---> $(LOCAL_DIR)"
	@bd=$(shell cmany show_build_names $(RTAGS_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	$(call copy_tree,$(RTAGS_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))

cquery_install: $(LOCAL_DIR) $(CQUERY_INSTALL_DIR)
	@echo "cquery_install: $(CQUERY_INSTALL_DIR) ---> $(LOCAL_DIR)"
	@bd=$(shell cmany show_build_names $(CQUERY_CMANY_ARGS)) ; \
	echo "Build name: $$bd" ; \
	$(call copy_tree,$(CQUERY_INSTALL_DIR)/$$bd,*,$(LOCAL_DIR))

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


$(RTAGS_INSTALL_DIR): $(RTAGS_BUILD_DIR)
	@echo "rtags_install_dir: $(RTAGS_INSTALL_DIR)"
	cd $(RTAGS_SRC_DIR) && cmany i $(RTAGS_CMANY_ARGS)

$(CQUERY_INSTALL_DIR): $(CQUERY_BUILD_DIR)
	@echo "cquery_install_dir: $(CQUERY_INSTALL_DIR)"
	cd $(CQUERY_SRC_DIR) && cmany i $(CQUERY_CMANY_ARGS)

$(CCLS_INSTALL_DIR): $(CCLS_BUILD_DIR)
	@echo "ccls_install_dir: $(CCLS_INSTALL_DIR)"
	cd $(CCLS_SRC_DIR) && cmany i $(CCLS_CMANY_ARGS)

$(CLANG_INSTALL_DIR): $(CLANG_BUILD_DIR)
	@echo "clang_install_dir: $(CLANG_INSTALL_DIR)"
	cd $(CLANG_SRC_DIR) && cmany b $(CLANG_CMANY_ARGS)


$(RTAGS_BUILD_DIR): cmany $(RTAGS_SRC_DIR)
	@echo "rtags_build_dir: $(RTAGS_BUILD_DIR)"
	cd $(RTAGS_SRC_DIR) && cmany c $(RTAGS_CMANY_ARGS)

$(CQUERY_BUILD_DIR): cmany $(CQUERY_SRC_DIR)
	@echo "cquery_build_dir: $(CQUERY_BUILD_DIR)"
	cd $(CQUERY_SRC_DIR) && cmany c $(CQUERY_CMANY_ARGS)

$(CCLS_BUILD_DIR): cmany $(CCLS_SRC_DIR)
	@echo "ccls_build_dir: $(CCLS_BUILD_DIR)"
	cd $(CCLS_SRC_DIR) && cmany c $(CCLS_CMANY_ARGS)

$(CLANG_BUILD_DIR): cmany $(CLANG_SRC_DIR)
	@echo "clang_build_dir: $(CLANG_INSTALL_DIR)"
	cd $(CLANG_SRC_DIR) && cmany c $(CLANG_CMANY_ARGS)


$(RTAGS_SRC_DIR): $(RTAGS_DIR)
	@echo "rtags_src_dir: $(RTAGS_SRC_DIR)"
	if [ ! -d "$(RTAGS_SRC_DIR)" ] ; then \
	    cd $(LOCAL_SRC_DIR) && git clone --recursive --branch=$(RTAGS_BRANCH) $(RTAGS_REPO) $(RTAGS_SRC_DIR) ; \
	fi

$(CQUERY_SRC_DIR): $(CQUERY_DIR)
	@echo "cquery_src_dir: $(CQUERY_SRC_DIR)"
	if [ ! -d "$(CQUERY_SRC_DIR)" ] ; then \
	    cd $(LOCAL_SRC_DIR) && git clone --recursive --branch=$(CQUERY_BRANCH) $(CQUERY_REPO) $(CQUERY_SRC_DIR) ; \
	fi

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

$(RTAGS_DIR):
	$(call makedirs, $(RTAGS_DIR))

$(CQUERY_DIR):
	$(call makedirs, $(CQUERY_DIR))

$(CCLS_DIR):
	$(call makedirs, $(CCLS_DIR))

$(CLANG_DIR):
	$(call makedirs, $(CLANG_DIR))

$(LOCAL_SRC_DIR):
	$(call makedirs, $(LOCAL_SRC_DIR))

$(LOCAL_DIR):
	$(call makedirs, $(LOCAL_DIR))

$(LOCAL_DIR)/bin:
	$(call makedirs, $(LOCAL_DIR)/bin)
