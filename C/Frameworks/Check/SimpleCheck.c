#include <stdio.h>
#include <stdlib.h>
#include <check.h>

int foo (int x) {
    return x*2;
}

START_TEST (test_foo_simple) {
    ck_assert_int_eq (foo(8), 16);
    ck_assert_int_eq (foo(4), 8);
    ck_assert_int_eq (foo(1), 2);
    ck_assert_int_eq (foo(2), 4);
    ck_assert_int_eq (foo(1), 2);
} END_TEST

Suite* fooSuite (void) {
    Suite *s;
    TCase *tcCore;

    s = suite_create ("Foo");

    /* Core test Case */
    tcCore = tcase_create ("Core");

    tcase_add_test (tcCore, test_foo_simple);
    suite_add_tcase (s, tcCore);

    return s;
}

int main () {
    int numberFailed;
    Suite *s;
    SRunner *sr;
    s = fooSuite ();
    sr = srunner_create (s);

    srunner_run_all (sr, CK_NORMAL);
    numberFailed = srunner_ntests_failed (sr);

    srunner_free (sr);

    return (numberFailed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;

    return 0;
}














