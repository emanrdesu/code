#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <libnotify/notify.h>

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

// pommy [session_time [break_time [extended_break_time]]]
// e.g. pommy 30 5 15 (default = 25 5 20)

// globals

enum timer_t{SESSION, BREAK, EXTENDED};

int  * timer;
char * timer_default[] = {"25", "5", "20"};

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
    if(stop) return NULL;

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


int ctoi(char c) {
  switch(c) {
  case '1': return 1; break;
  case '2': return 2; break;
  case '3': return 3; break;
  case '4': return 4; break;
  case '5': return 5; break;
  case '6': return 6; break;
  case '7': return 7; break;
  case '8': return 8; break;
  case '9': return 9; break;
  }
}

int digitp(char c) {
  return c >= 48 && c <= 57;
}

int integerp(char * str) {
  for(int i = 0; str[i] != '\0'; i++)
    if(!digitp(str[i])) return 0;

  return 1;
}

int * get_digits(char * number) {
  int size = strlen(number) + 1;
  int * digits = malloc(size * sizeof(int));

  digits[0] = size;

  for(int i = 1; i < size; i++)
    digits[i] = ctoi(number[i]);

  return digits;
}


int main(int argc, char ** argv) {

  // command line processing

  if(argc > 4) {
    fprintf(stderr, "Error: pommy takes 3 arguments max.\n");
    return 1;
  }

  for(int i = 1; i < argc; i++)
    if(!integerp(argv[i])) {
      fprintf(stderr, "Error: '%s' is not an integer\n", argv[i]);
      return 1;
    }

  if (argc >= 2)
    timer_default[SESSION] = argv[1];

  if (argc >= 3)
    timer_default[BREAK] = argv[2];

  if (argc == 4)
    timer_default[EXTENDED] = argv[3];


  // initialization
  // ncurses
  initscr();
  raw();
  noecho();

  // libnotify
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
