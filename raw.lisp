;;;; A perfect clone of the cfmakeraw function from termios.c

(defun raw ()
  (let ((termios-instance (sb-posix:tcgetattr sb-sys:*stdin*)))
    (setf (sb-posix:termios-iflag termios-instance)
	  (logand (sb-posix:termios-iflag termios-instance)
		  (lognot (logior sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk sb-posix:istrip
				  sb-posix:inlcr sb-posix:igncr sb-posix:icrnl sb-posix:ixon))))
    (setf (sb-posix:termios-oflag termios-instance)
	  (logand (sb-posix:termios-oflag termios-instance)
		  (lognot sb-posix:opost)))
    (setf (sb-posix:termios-lflag termios-instance)
	  (logand (sb-posix:termios-lflag termios-instance)
		  (lognot (logior sb-posix:echo sb-posix:echonl sb-posix:icanon
				  sb-posix:isig sb-posix:iexten))))
    (setf (sb-posix:termios-cflag termios-instance)
	  (logand (sb-posix:termios-cflag termios-instance)
		  (lognot (logior sb-posix:csize sb-posix:parenb))))
    (setf (sb-posix:termios-cflag termios-instance)
	  (logior (sb-posix:termios-cflag termios-instance) sb-posix:cs8))
    (sb-posix:tcsetattr sb-sys:*stdin* sb-posix:tcsanow termios-instance)))
