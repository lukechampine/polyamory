#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* global vars */
unsigned char *program, *array, *pointer;
int arraysize;

/* double array size */
void resizeArray() {
    long int offset = pointer - array;
    array = realloc(array, arraysize*2);
    pointer = array + offset; /* reset pointer position */
    arraysize *= 2;
}

/* check for unmatched brackets */
int checkbrackets() {
    unsigned char *progptr = program;
    int count = 0;
    while (*progptr != '\0' && count >= 0) {
        if (*progptr == '[') ++count;
        if (*progptr == ']') --count;
        ++progptr;
    }
    return (count > 0) - (count < 0); /* signum */
}

/* recursively seek to matching [ ] */
void seekloop(char c) {
    int dir = '\\' - c; /* 1 or -1, since \ lies between [ and ] */
    while (program += dir) {
        if (*program == c) {
            seekloop(c);
            program += dir;
        }
        if (*program == '\\' + dir) /* complement of c */
            return;
    }
}

/* main interpreter function */
int interpret() {
    int c;
    do {
        switch (*program) {
            case '>': if (pointer - array == arraysize - 1) resizeArray(); ++pointer; continue;
            case '<': if (pointer == array) return 1; --pointer; continue;
            case '+': ++*pointer; continue;
            case '-': --*pointer; continue;
            case '.': putchar(*pointer); continue;
            case ',': if ((c = getchar()) > 0) *pointer = c; continue;
            case '[': if (*pointer == 0) seekloop('['); continue;
            case ']': if (*pointer != 0) seekloop(']'); continue;
            default: continue;
        }
    } while (*++program != '\0');
    putchar('\n');
    return 0;
}

int main() {
    /* read stdin into program string */
    program = calloc(512, 1);
    unsigned char buffer[80];
    while (fgets(buffer, 80, stdin)) {
        program = realloc(program, strlen(program) + strlen(buffer) + 1);
        strcat(program, buffer);
    }
    /* check for syntax errors */
    switch (checkbrackets()) {
        case 1:  puts("Error: unmatched ["); return 1;
        case -1: puts("Error: unmatched ]"); return 1;
    }
    /* initialize array */
    arraysize = 512;
    array = malloc(arraysize);
    /* initialize pointer */
    pointer = array;
    /* run interpreter */
    if (interpret()) {
        puts("Error: out of bounds");
        return 1;
    }
    return 0;
}
