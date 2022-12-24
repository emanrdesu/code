#include <stdio.h>
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <libnotify/notify.h>

#define atomic(x) pthread_mutex_lock(&mutex); x; pthread_mutex_unlock(&mutex)

// pommy is a pomodoro technique ncurses program with notifications
// displays pretty digits, allows interaction with timer

// interact keys:
// j (decrement timer), k (increment timer),
// - (decrease timer size), = (decrease timer size)
// space (pause/unpause timer), q (quit)

// compile command:
// gcc -o pommy `pkg-config --cflags --libs libnotify ncurses` pommy.c

// pommy [session_time [break_time [extended_break_time]]]
// e.g. pommy 30 5 15 (default = 25 5 20)


/* GLOBALS */

enum timer_t{SESSION, BREAK, EXTENDED};

int  * timer, * drawnp;
char * timer_default[] = {"25", "5", "20"};
char * message[] = { NULL, NULL };

int session = 1;
int breakp = 0;

int scale, scale_max;

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


/* PROCEDURES */

/* utility */

void set_stop(int new) {
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

int min_pretty_width(int size) {
  return size * (2 * 4) + 2;
}

void notify(char * header, char * body) {
  NotifyNotification * notification = notify_notification_new (header, body, "dialog-information");
  notify_notification_show (notification, NULL);
  g_object_unref(G_OBJECT(notification));
}


/* timer fiddling */

int * create_timer(char * number) {
  int size = strlen(number) + 1 + 2;
  int * timer = malloc(size * sizeof(int));

  timer[0] = --size;
  (timer+1)[size-1] = (timer+1)[size-2] = 0;

  for(int i = 0; i < (size-2); i++)
    (timer+1)[i] = ctoi(number[i]);

  return timer;
}

int timer_finishedp() {
  int zerop = 1;
  for(int i = 0; i < timer[0]; i++)
    zerop = zerop && !(timer+1)[i];

  return zerop;
}

void decrement(int * number, int size) {
  int i = size - 1;

  while(!number[i] && i>=0)
    number[i--] = 9;

  number[i]--;
}

void decrement_timer() {
  int size = timer[0];

  if(timer_finishedp()) return;

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

  pthread_mutex_lock(&mutex);

  if(!timer_finishedp()) {
    decrement_timer();
    goto unlock;
  }

  if(breakp) {
    session++; breakp = 0;

    free(timer);
    timer = create_timer(timer_default[SESSION]);
    redrawp = 1; clear();

    sprintf(message[0], "%s min", timer_default[SESSION]);
    notify("Begin Session", message[0]);
  }
  else {
    breakp = 1;
    free(timer);
    int type = (session % 4) ? BREAK : EXTENDED;

    sprintf(message[0], "Begin%s Break", type == BREAK ? "" : " Extended");
    sprintf(message[1], "%s min", timer_default[type]);
    notify(message[0], message[1]);

    timer = create_timer(timer_default[type]);
    redrawp = 1; clear();
  }

 unlock:
  pthread_mutex_unlock(&mutex);
}


/* drawing procedures */

void draw_timer_text() {
  int size = timer[0];
  int height, width;
  getmaxyx(stdscr, height, width);

  move(height / 2, (width - size - 1) / 2);

  for(int i = 0 ; i < (size - 2); i++)
      printw("%d", (timer+1)[i]);

  addch(':');
  printw("%d", (timer+1)[size-2]);
  printw("%d", (timer+1)[size-1]);
}

void draw_cell(char * c, int scale, int y, int x) {
  for(int cy = 0; cy < scale; cy++)
    for(int cx = 0; cx < 2 * scale; cx++)
      mvaddstr(y + cy, x + cx, c);
}

void draw_digit(int scale, int y, int x, int index) {
  if(redrawp || drawnp[index] != (timer+1)[index]) {
    drawnp[index] = (timer+1)[index];

    int cellX, cellY, drawp;
    for(int cell = 0; cell < 15; cell++) {
      cellX = x + ((cell % 3) * (2 * scale));
      cellY = y + ((cell / 3) * scale);
      drawp = digit[drawnp[index]] & (1 << (14 - cell));

      draw_cell(drawp ? "█" : " ", scale, cellY, cellX);
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

  int timer_width = min_pretty_width(size) * scale;
  int startX = (width - timer_width) / 2 - scale;
  int startY = (height - (5 * scale)) / 2;

  int i;
  for(i = 0; i < size-2; i++)
    draw_digit(scale, startY,
               startX + (i * 2 * scale * 4 + 1), i);

  startX += (i * 2 * scale * 4 + 1);
  draw_colon(scale, startY, startX + 1);
  startX += (2 * scale * 2 + 1);

  for(int i = 0; i < 2; i++)
    draw_digit(scale, startY,
               startX + (i * 2 * scale * 4 + 1), size-2+i);
}

void draw_timer() {
  int size = timer[0];

  int height, width;
  getmaxyx(stdscr, height, width);

  int scaleX = width / min_pretty_width(size);
  int scaleY = height / 5;

  int scaleYX = MIN(scaleX, scaleY);
  scale_max = scaleYX;
  if(scale == 0) scale = scaleYX / 2;
  scaleYX = MIN(scaleYX, scale);

  if(scaleYX)
    draw_timer_pretty(scaleYX);
  else
    draw_timer_text();

  redrawp = 0;
  refresh();
}

void * draw_worker(void * vargp) {
  redrawp = 1;

  while(1) {
    if(stopp) {
      redrawp = 1;
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


/* MAIN */

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

  message[0] = malloc(50);
  message[1] = malloc(50);

  // ncurses
  setlocale(LC_ALL, "");
  initscr();
  raw();
  noecho();
  curs_set(0);

  // libnotify
  notify_init ("pommy");

  sprintf(message[0], "%s min", timer_default[SESSION]);
  notify("Begin Session", message[0]);

  timer = create_timer(timer_default[SESSION]);
  drawnp = malloc(timer[0] * sizeof(int));
  for(int i = 0; i < timer[0]; i++)
    drawnp[i] = -1;

  pthread_t draw_thread;
  create_worker(&draw_thread);


  /* main event loop */

  while(1) {
    switch(getch()) {

    case ' ':
      set_stop(!stopp);

      if (!stopp)
        create_worker(&draw_thread);

      break;

    case 'j':
      atomic(decrement_timer());
      break;

    case 'k':
      atomic(increment_timer());
      break;

    case '-':
      atomic(scale = MAX(scale-1, 0));
      clear(); redrawp = 1;
      break;

    case '=':
      atomic(scale = MIN(scale+1, scale_max));
      clear(); redrawp = 1;
      break;

    case 'q':
      goto leave;
      break;

    // resize
    case 410:
      set_stop(1);
      pthread_join(draw_thread, NULL);
      clear();
      usleep(500);
      set_stop(0);
      create_worker(&draw_thread);
      break;
    }
  }


 leave:
  set_stop(1);
  pthread_join(draw_thread, NULL);

  free(timer); free(drawnp);
  free(message[0]); free(message[1]);

  endwin();        // ncurses
  notify_uninit(); // libnotify

  return 0;
}
