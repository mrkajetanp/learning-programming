include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory (ToDoCore)

add_executable (toDo main.cpp)
target_link_libraries (toDo toDoCore)
add_test (toDoTest toDo)
