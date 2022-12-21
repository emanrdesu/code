#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <libnotify/notify.h>

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

// pommy [session_time [break_time [extended_break_time]]]
// e.g. pommy 30 5 15 (default = 25 5 20)

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
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;


void * draw_worker(void * vargp) {
  while(1) {
    if(stop) {
      pthread_mutex_unlock(&mutex);
      return NULL;
    }

    // draw digits
    sleep(1);
    // decrement timer
  }
}

void create_worker(pthread_t * ptp) {
  pthread_create(ptp, NULL, draw_worker, NULL);
}

void set_stop(int new) {
  pthread_mutex_lock(&mutex);
  stop = new;
  pthread_mutex_unlock(&mutex);
}


int main(int argc, char ** argv) {
  // ncurses initialization
  initscr();
  raw();
  noecho();

  // libnotify initialization
  notify_init ("pommy");


  pthread_t draw_thread;
  create_worker(&draw_thread);

  while(1) {
    switch(getch()) {

    // space
    case 32:
      set_stop(!stop);

      if (!stop)
        create_worker(&draw_thread);

      break;

    // q
    case 113:
      goto leave;
      break;

    // resize
    case 410:
      set_stop(1);
      pthread_join(draw_thread, NULL);
      clear(); refresh();
      set_stop(0);
      create_worker(&draw_thread);
      break;
    }
  }

 leave:
  endwin();        // ncurses
  notify_uninit(); // libnotify

	return 0;
}
