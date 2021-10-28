import os
import sys
import shlex
import subprocess


EMACS_DIR = os.path.abspath(os.path.dirname(__file__))
LOCAL_DIR = f"{EMACS_DIR}/local"
if os.path.exists(f"{EMACS_DIR}/.local"):
    with open(f"{EMACS_DIR}/.local") as f:
        LOCAL_DIR = string(f.read()).strip()


WINDOWS = "Windows"
UBUNTU = "ubuntu"
ARCHLINUX = "archlinux"

if os.path.exists("/etc/arch-release"):
    platform = ARCHLINUX


def setkw(obj, kwargs, keys):
    for key in keys:
        setattr(obj, key, kwargs.get(key))


def setdefaults(kwargs, **defaults):
    for k, v in defaults.items():
        if kwargs.get(k) is None:
            kwargs[k] = v
    return kwargs


def runcmd(cmd, *args, **kwargs):
    cmd = shlex.split(shlex.join(cmd, *args))
    log(f"> {cmd}")
    kwargs = setdefaults(kwargs, check=True)
    return subprocess.run(cmd, **kwargs)


def log(*args, **kwargs):
    kwargs = setdefaults(kwargs, flush=True)
    print(*args, **kwargs)


def apt_repo(repo):
    runcmd(f"add-apt-repository {repo}")
    runcmd(f"apt-get update")


def aptget_install(pkg):
    runcmd(f"apt-get install -y {pkg}")


def pacman_install(pkg):
    runcmd(f"pacman -S {pkg}")


def yaourt_install(pkg):
    runcmd(f"yaourt -S {pkg}")


def pip_install(pkg):
    runcmd(f"trizen -S {pkg}")


class App:

    def download(self):
        pass

    def clone(self):
        pass

    def config(self):
        pass

    def compile(self):
        pass

    def install(self):
        pass


class PipPackage(App):

    def __init__(self, **kwargs):
        setkw(self, kwargs, ('name', 'pkg'))
        if self.pkg is None:
            self.pkg = self.name

    def install(self):
        pass


class StandardApp(App):

    def __init__(self, **kwargs):
        setkw(self, kwargs, ('name', 'version', 'winurl', 'apt', 'apt_repo', 'pacman'))

    def install(self):
        pass

    def _install_windows(self):
        if self.version is not None:
            self.winurl = self.winurl.format(version=self.version)

    def _install_ubuntu(self):
        if self.apt is not None:
            if self.apt_repo is not None:
                apt_repo(self.apt_repo)
            aptget_install(self.apt)

    def _install_pacman(self):
        if self.pacman is not None:
            pacman_install(self.pacman)
        elif self.trizen is not None:
            trizen_install(self.trizen)


class CompileableApp(App):

    def __init__(self, **kwargs):
        setkw(self, kwargs, ('name', 'repo', 'ubuntu', 'pacman'))

    def install(self):
        pass


def mkapps(*app_objs):
    apps = object()
    apps.all = app_objs
    for obj in app_objs:
        setattr(apps, obj.name, obj)
    return apps


def app(name, cls, **kwargs):
    return cls(name=name, **kwargs)


apps = mkapps(
    app('cmany', PipPackage),
    app('pillow', PipPackage),
    app('grip', PipPackage),
    app('bazel', StandardApp, winurl="https://github.com/bazelbuild/bazel/releases/download/3.6.0/bazel-3.6.0-windows-x86_64.exe"),
    app('ripgrep', StandardApp,
        winurl="https://github.com/BurntSushi/ripgrep/releases/download/12.0.1/ripgrep-12.0.1-i686-pc-windows-msvc.zip",
        apt="ripgrep", apt_repo="ppa:x4121/ripgrep",
        pacman="ripgrep"),
    app('ag', StandardApp,
        winurl="https://github.com/k-takata/the_silver_searcher-win32/releases/download/2019-03-23%2F2.2.0-19-g965f71d/ag-2019-03-23_2.2.0-19-g965f71d-x64.zip",
        apt="the_silver_searcher", apt_repo="pgolm/the-silver-searcher",
        pacman="the_silver_searcher"),
)


if __name__ == "__main__":
    for app in apps.all:
        app.install()
