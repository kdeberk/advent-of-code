%ifndef __CONSTANTS__
%define __CONSTANTS__


;; Basic base pointer helpers
%define SINGLE_ARG (ebp+8)

%define FIRST_OF_TWO_ARGS (ebp+12)
%define SECOND_OF_TWO_ARGS (ebp+8)

%define FIRST_OF_THREE_ARGS (ebp+16)
%define SECOND_OF_THREE_ARGS (ebp+12)
%define THIRD_OF_THREE_ARGS (ebp+8)

%define FIRST_OF_FOUR_ARGS (ebp+20)
%define SECOND_OF_FOUR_ARGS (ebp+16)
%define THIRD_OF_FOUR_ARGS (ebp+12)
%define FOURTH_OF_FOUR_ARGS (ebp+8)

%define FIRST_VAR (ebp-4)
%define SECOND_VAR (ebp-8)
%define THIRD_VAR (ebp-12)
%define FOURTH_VAR (ebp-16)

%endif
