#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <libnotify/notify.h>

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

// globals
int timer[] = {2, 5, 0, 0};

int digit[] = {
  0x7B6F,
  0x2492,
  0x73E7,
  0x7ECF,
  0x5BC9,
  0x79CF,
  0x79EF,
  0x7249,
  0x7BEF,
  0x7BC9
};

int stop = 0;


int main(int argc, char ** argv) {
  // ncurses initialization
  initscr();
  raw();
  noecho();

  // libnotify initialization
  notify_init ("pommy");

  while(1) {
    switch(getch()) {
    case 32: // space
      // code
      break;
    case 113: // q
      goto leave;
      break;
    case 410: // resize
      // code
      break;
    }
  }

 leave:
  // wrap up
  endwin();        // ncurses
  notify_uninit(); // libnotify

	return 0;
}
