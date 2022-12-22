#include <stdio.h>
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <libnotify/notify.h>

#define atomic(x) pthread_mutex_lock(&mutex); x; pthread_mutex_unlock(&mutex)

// pommy is a pomodoro technique ncurses program
// displays ascii timer, allows pausing

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

// pommy [session_time [break_time [extended_break_time]]]
// e.g. pommy 30 5 15 (default = 25 5 20)

/* globals */

enum timer_t{SESSION, BREAK, EXTENDED};

int  * timer;
int  * drawnp;
char * timer_default[] = {"25", "5", "20"};

int session = 1;
int breakp = 0;

int digit[] = {
  0x7B6F,
  0x2492,
  0x73E7,
  0x73CF,
  0x5BC9,
  0x79CF,
  0x79EF,
  0x7249,
  0x7BEF,
  0x7BC9
};

int stopp = 0;
int redrawp = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

/* procedures */

void * draw_worker(void * vargp) {
  while(1) {
    if(stopp) {
      draw_timer();
      return NULL;
    }

    draw_timer();
    sleep(1);
    update_timer();
  }
}

void create_worker(pthread_t * ptp) {
  pthread_create(ptp, NULL, draw_worker, NULL);
}

void set_stopp(int new) {
  atomic(stopp = new);
}

int ctoi(char c) {
  return c - 48;
}

int digitp(char c) {
  return c >= 48 && c <= 57;
}

int integerp(char * str) {
  for(int i = 0; str[i] != '\0'; i++)
    if(!digitp(str[i])) return 0;

  return 1;
}

int * get_timer(char * number) {
  int size = strlen(number) + 1 + 2;
  int * ret = malloc(size * sizeof(int));

  ret[0] = --size;
  (ret+1)[size-1] = (ret+1)[size-2] = 0;

  for(int i = 0; i < (size-2); i++)
    (ret+1)[i] = ctoi(number[i]);

  return ret;
}


void decrement(int * number, int size) {
  int i = size - 1;

  while(!number[i] && i>=0)
    number[i--] = 9;

  number[i]--;
}

void decrement_timer() {
  int size = timer[0];

  if(!(timer+1)[size-1] && !(timer+1)[size-2]) {
    (timer+1)[size-2] = 5;
    (timer+1)[size-1] = 9;

    decrement(timer+1, size-2);
  }
  else
    decrement(timer+1+(size-2), 2);
}

void increment(int * number, int size) {
  int i = size - 1;
  while(number[i] == 9 && i>=0)
    number[i--] = 0;

  number[i]++;
}

void increment_timer() {
  int size = timer[0];

  if((timer+1)[size-2] == 5 && (timer+1)[size-1] == 9) {
    (timer+1)[size-2] = (timer+1)[size-1] = 0;

    increment(timer+1, size-2);
  }
  else
    increment(timer+1+(size-2), 2);
}

void update_timer() {
  int size = timer[0];

  int zerop = 1;
  for(int i = 0; i < size; i++)
    zerop = zerop && !(timer+1)[i];

  pthread_mutex_lock(&mutex);

  if(zerop) {
    if(breakp) {
      session++; breakp = 0;

      free(timer);
      timer = get_timer(timer_default[SESSION]);
    }
    else {
      breakp = 1;
      free(timer);
      int type = (session % 4) ? BREAK : EXTENDED;
      timer = get_timer(timer_default[type]);
    }
  }
  else
    decrement_timer();

  pthread_mutex_unlock(&mutex);
}


void draw_timer_text() {
  int size = timer[0];
  int height, width;
  getmaxyx(stdscr, height, width);

  move(height / 2, (width - timer[0] - 1) / 2);

  for(int i = 0 ; i < timer[0] - 2; i++)
      printw("%d", (timer+1)[i]);

  addch(':');
  printw("%d", (timer+1)[size-2]);
  printw("%d", (timer+1)[size-1]);
}

int minspace(int size) {
  return size * (2 * 4) + 2;
}

void draw_cell(char * c, int scale, int y, int x) {
  for(int cy = 0; cy < scale; cy++)
    for(int cx = 0; cx < 2 * scale; cx++)
      mvaddstr(y + cy, x + cx, c);
}

void draw_digit(int scale, int y, int x, int index) {
  if(redrawp || drawnp[index] != (timer+1)[index]) {
    drawnp[index] = (timer+1)[index];

    int cellX, cellY, onp;
    for(int cell = 0; cell < 15; cell++) {
      cellX = x + ((cell % 3) * (2 * scale));
      cellY = y + ((cell / 3) * scale);
      onp = digit[drawnp[index]] & (1 << (14 - cell));

      draw_cell(onp ? "█" : " ", scale, cellY, cellX);
    }
  }
}

void draw_colon(int scale, int y, int x) {
  draw_cell("█", scale, y + scale, x);
  draw_cell("█", scale, y + scale * 3, x);
}

void draw_timer_pretty(int scale) {
  int size = timer[0];

  int height, width;
  getmaxyx(stdscr, height, width);

  int timer_width = minspace(size) * scale;
  int startX = (width - timer_width) / 2;
  int startY = (height - (5 * scale)) / 2;

  int i;
  for(i = 0; i < size-2; i++)
    draw_digit(scale, startY,
               startX + (i * 2 * scale * 4 + 1), i);

  startX += (i * 2 * scale * 4 + 1);
  draw_colon(scale, startY, startX);
  startX += (2 * scale * 2 + 1);

  for(int i = 0; i < 2; i++)
    draw_digit(scale, startY,
               startX + (i * 2 * scale * 4 + 1), size-2+i);
}

void draw_timer() {
  int size = timer[0];

  int height, width;
  getmaxyx(stdscr, height, width);

  int scaleX = width / minspace(size);
  int scaleY = height / 5;

  int scale = MIN(scaleX, scaleY);

  if(scale)
    draw_timer_pretty(scale);
  else
    draw_timer_text();

  redrawp = 0;
  refresh();
}


int main(int argc, char ** argv) {

  /* command line processing */

  if(argc > 4) {
    fprintf(stderr, "Error: pommy takes 3 arguments max.\n");
    return 1;
  }

  for(int i = 1; i < argc; i++)
    if(!integerp(argv[i])) {
      fprintf(stderr, "Error: '%s' is not a positive integer\n", argv[i]);
      return 1;
    }

  if (argc >= 2)
    timer_default[SESSION] = argv[1];

  if (argc >= 3)
    timer_default[BREAK] = argv[2];

  if (argc == 4)
    timer_default[EXTENDED] = argv[3];


  /* initialization */

  // ncurses
  setlocale(LC_ALL, "");
  initscr();
  raw();
  noecho();
  start_color();
  init_pair(1, COLOR_MAGENTA, COLOR_BLACK);
  curs_set(0);

  // libnotify
  notify_init ("pommy");

  timer = get_timer(timer_default[SESSION]);
  drawnp = malloc(timer[0] * sizeof(int));
  for(int i = 0; i < timer[0]; i++)
    drawnp[i] = -1;

  pthread_t draw_thread;
  create_worker(&draw_thread);


  /* main event loop */

  while(1) {
    switch(getch()) {

    case ' ':
      attron(COLOR_PAIR(1));
      redrawp = 1;
      set_stopp(!stopp);

      if (!stopp) {
        attroff(COLOR_PAIR(1));
        redrawp = 1;
        create_worker(&draw_thread);
      }

      break;

    case 'j':
      atomic(decrement_timer());
      break;

    case 'J':
      atomic (
        for(int i = 0; i < 60; i++)
          decrement_timer()
      );

      break;

    case 'k':
      atomic(increment_timer());
      break;

    case 'K':
      atomic(
        for(int i = 0; i < 60; i++)
          increment_timer()
      );

      break;

    case 'q':
      goto leave;
      break;

    // resize
    case 410:
      set_stopp(1);
      pthread_join(draw_thread, NULL);
      clear(); refresh();
      usleep(500);
      set_stopp(0);
      redrawp = 1;
      create_worker(&draw_thread);
      break;
    }
  }


 leave:
  set_stopp(1);
  pthread_join(draw_thread, NULL);
  free(timer); free(drawnp);

  endwin();        // ncurses
  notify_uninit(); // libnotify

	return 0;
}
