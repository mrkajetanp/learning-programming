
CPPFLAGS += -std=c++14

CXXFLAGS += -g -Wall -Wextra

TARGETS = SmartPointers

all : $(TARGETS)

clean :
	rm -f SmartPointers

run :
	./SmartPointers

SmartPointers : SmartPointers.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $@.cpp
