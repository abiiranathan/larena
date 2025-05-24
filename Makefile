CC = gcc
AS = as
CFLAGS = -O3 -Wall -Wextra -std=c99 -fPIC
ASFLAGS = --64
LDFLAGS = -shared

SRCDIR = src
INCDIR = include
TESTDIR = test
OBJDIR = obj
LIBNAME = liblarena

# Source files
ASM_SOURCES = $(SRCDIR)/larena.s
C_SOURCES = $(SRCDIR)/larena_c.c
TEST_SOURCES = $(TESTDIR)/test_larena.c

# Object files
ASM_OBJECTS = $(ASM_SOURCES:$(SRCDIR)/%.s=$(OBJDIR)/%.o)
C_OBJECTS = $(C_SOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
ALL_OBJECTS = $(ASM_OBJECTS) $(C_OBJECTS)

# Targets
STATIC_LIB = $(LIBNAME).a
SHARED_LIB = $(LIBNAME).so
TEST_EXEC = test_larena

.PHONY: all clean test install static shared

all: static shared test

static: $(STATIC_LIB)

shared: $(SHARED_LIB)

# Create object directory
$(OBJDIR):
	mkdir -p $(OBJDIR)

# Compile assembly sources
$(OBJDIR)/%.o: $(SRCDIR)/%.s | $(OBJDIR)
	$(AS) $(ASFLAGS) -o $@ $<

# Compile C sources  
$(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)
	$(CC) $(CFLAGS) -I$(INCDIR) -c -o $@ $<

# Create static library
$(STATIC_LIB): $(ALL_OBJECTS)
	ar rcs $@ $^

# Create shared library
$(SHARED_LIB): $(ALL_OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^

# Build test
$(TEST_EXEC): $(TEST_SOURCES) $(STATIC_LIB)
	$(CC) $(CFLAGS) -I$(INCDIR) -o $@ $< -L. -llarena

test: $(TEST_EXEC)
	./$(TEST_EXEC)

# Install library (requires sudo)
install: $(STATIC_LIB) $(SHARED_LIB)
	cp $(STATIC_LIB) /usr/local/lib/
	cp $(SHARED_LIB) /usr/local/lib/
	cp $(INCDIR)/larena.h /usr/local/include/
	ldconfig

clean:  
	rm -rf $(OBJDIR) $(STATIC_LIB) $(SHARED_LIB) $(TEST_EXEC)

# Debug build
debug: CFLAGS += -g -DDEBUG
debug: ASFLAGS += -g
debug: all
