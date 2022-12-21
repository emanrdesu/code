#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <libnotify/notify.h>

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

int main() {
  // ncurses initialization
  initscr();
  raw();
  noecho();

  // libnotify initialization
  notify_init ("pommy");

  // wrap up
  endwin();        // ncurses
  notify_uninit(); // libnotify
	return 0;
}
