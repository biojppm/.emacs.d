import sys
import os.path
from PIL import ImageFile, ImageGrab


def save_clipboard_to_file(filename):
    f, ext = os.path.splitext(filename)
    ImageFile.LOAD_TRUNCATED_IMAGES = True
    im = ImageGrab.grabclipboard()
    if not im:
        raise Exception('clipboard buffer is not image!')
    im.save(filename)


if __name__ == '__main__':
    print(sys.argv)
    save_clipboard_to_file(sys.argv[1])
