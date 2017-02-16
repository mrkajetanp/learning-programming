
CPPFLAGS += -std=c++14

CXXFLAGS += -g -Wall -Wextra

TARGETS = NLimitsTypeTraitsUtils

all : $(TARGETS)

clean :
	rm -f NLimitsTypeTraitsUtils

run :
	./NLimitsTypeTraitsUtils

NLimitsTypeTraitsUtils : NLimitsTypeTraitsUtils.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $@.cpp
