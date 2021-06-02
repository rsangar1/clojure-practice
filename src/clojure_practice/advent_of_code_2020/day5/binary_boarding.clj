(ns clojure-practice.advent-of-code-2020.day5.binary-boarding
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  " --- Day 5: Binary Boarding ---
  You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which seat is yours,
  and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.

  You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input) ;
  perhaps you can find your seat through process of elimination.

  Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like
   FBFBBFFRLR, where F means  front, B means back, L means left, and R means right.

  The first 7 characters will either be F or B             ; these specify exactly one of the 128 rows on the plane
  (numbered 0 through 127). Each letter tells you which half of a region the given seat is in.
  Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the
  back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left
  with exactly one row.

  For example, consider just the first seven characters of FBFBBFFRLR:

  Start by considering the whole range, rows 0 through 127.
  F means to take the lower half, keeping rows 0 through 63.
  B means to take the upper half, keeping rows 32 through 63.
  F means to take the lower half, keeping rows 32 through 47.
  B means to take the upper half, keeping rows 40 through 47.
  B keeps rows 44 through 47.
  F keeps rows 44 through 45.
  The final F keeps the lower of the two, row 44.
  The last three characters will be either L or R          ;
  these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above
  proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.

  For example, consider just the last 3 characters of FBFBBFFRLR:

  Start by considering the whole range, columns 0 through 7.
  R means to take the upper half, keeping columns 4 through 7.
  L means to take the lower half, keeping columns 4 through 5.
  The final R keeps the upper of the two, column 5.
  So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

  Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID
  44 * 8 + 5 = 357.

  Here are some other boarding passes:

  BFFFBBFRRR: row 70, column 7, seat ID 567.
  FFFBBBFRRR: row 14, column 7, seat ID 119.
  BBFFBBFRLL: row 102, column 4, seat ID 820.
  As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?

  --- Part Two --
  Ding! The \"fasten seat belt\" signs have turned on. Time to find your seat.

  It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a
  catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing
  from your list as well.\n\nYour seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from
  yours will be in your list.\n\nWhat is the ID of your seat?
  ")

(defn read-input-file
  [path]
  (slurp path))

(defn ^:private binary-str->decimal
  "converts given binary string into a decimal number"
  [binary-str]
  (read-string (str "2r" binary-str)))

(defn ^:private char->binary-char
  [ch]
  (case ch
    \B 1
    \R 1
    \F 0
    \L 0
    "invalid input"))

(defn input-str->binary-str
  [input-str]
  (loop [xs     (vec input-str)
         result []]
    ;;(println (str "xs: " xs " result: " result))
    (if xs
      (recur (next xs)                                      ;;(rest xs) returns empty seq if no elements after first
             (conj result (char->binary-char (first xs))))
      (apply str result))))

(defn get-seat-coordinates
  "derive seat coordinates based on the input string and row, col substrings"
  [input-str row-begin row-end col-begin col-end]
  (let [binary-str       (input-str->binary-str input-str)
        row-str          (subs binary-str row-begin row-end)
        col-str          (subs binary-str col-begin col-end)
        row              (binary-str->decimal row-str)
        col              (binary-str->decimal col-str)
        seat-coordinates {:input-str  input-str
                          :binary-str binary-str
                          :row        row
                          :col        col}
        _                (println seat-coordinates)]
    seat-coordinates))

(defn ^:private calculate-seat-id*
  "calculate seat id based on the multiplier and the seat coordinates"
  [multiplier seat-coordinates]
  (let [row (:row seat-coordinates)
        col (:col seat-coordinates)]
    (+ (* row multiplier) col)))

(defn calculate-seat-ids
  [input-file-path]
  (let [input             (read-input-file input-file-path)
        seats             (str/split-lines input)
        seats-coordinates (map #(get-seat-coordinates % 0 7 7 10) seats)
        seat-ids          (map #(calculate-seat-id* 8 %) seats-coordinates)
        ;_                 (apply print seat-ids)
        ]
    seat-ids))

(defn find-highest-seat-id
  "highest seat id among given seats"
  [input-file-path]
  (let [seat-ids (calculate-seat-ids input-file-path)
        _        (apply print "all seat ids: " seat-ids)]
    (apply max seat-ids)))

;;-----------------------------part-2----------------------------
#_(defn find-missing-seat-id
    [input-file-path]
    (let [seat-ids           (calculate-seat-ids input-file-path)
          hightest-seat-id   (apply max seat-ids)
          lowest-seat-id     (apply min seat-ids)
          all-seat-ids-set   (set (range lowest-seat-id hightest-seat-id))
          input-seat-ids-set (set seat-ids)
          missing-seat-id    (set/difference all-seat-ids-set input-seat-ids-set)]
      (first missing-seat-id)))

(defn find-missing-seat-id
  [input-file-path]
  (let [seat-ids              (calculate-seat-ids input-file-path)
        hightest-seat-id      (apply max seat-ids)
        lowest-seat-id        (apply min seat-ids)
        all-seat-ids          (range lowest-seat-id (inc hightest-seat-id))
        sum-of-all-seat-ids   (apply + all-seat-ids)
        sum-of-given-seat-ids (apply + seat-ids)
        ;_                     (print (str "sum of all seat ids: " sum-of-all-seat-ids " and sum of given seat ids: " sum-of-given-seat-ids))
        ]
    (- sum-of-all-seat-ids sum-of-given-seat-ids)))


(comment
  #_(binary-str-to-decimal "00111")
  (char->binary-char \F)
  #_=> 0
  (char->binary-char \R)
  #_=> 1

  (input-str->binary-str "BFFFBBFRRR")
  #_=> "1000110111"

  (get-seat-coordinates "BFFFBBFRRR" 0 7 7 10)
  #_=> {:input-str "BFFFBBFRRR", :binary-str "1000110111", :row 70, :col 7}

  (calculate-seat-id* 8 {:input-str "BFFFBBFRRR", :binary-str "1000110111", :row 70, :col 7})
  #_=> 567

  #_(apply max [2 3 4])

  (calculate-seat-ids "resources/day5/sample-seats-input.txt")
  #_=> [357 567 119 820]

  (find-highest-seat-id "resources/day5/sample-seats-input.txt")
  #_=> 820
  (calculate-seat-ids "resources/day5/aoc-input1.txt")

  (find-highest-seat-id "resources/day5/aoc-input1.txt")
  #_=> 861

  (print (sort (calculate-seat-ids "resources/day5/aoc-input1.txt")))
  #_(100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 497 498 499 500 501 502 503 504 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563 564 565 566 567 568 569 570 571 572 573 574 575 576 577 578 579 580 581 582 583 584 585 586 587 588 589 590 591 592 593 594 595 596 597 598 599 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 624 625 626 627 628 629 630 631 632 634 635 636 637 638 639 640 641 642 643 644 645 646 647 648 649 650 651 652 653 654 655 656 657 658 659 660 661 662 663 664 665 666 667 668 669 670 671 672 673 674 675 676 677 678 679 680 681 682 683 684 685 686 687 688 689 690 691 692 693 694 695 696 697 698 699 700 701 702 703 704 705 706 707 708 709 710 711 712 713 714 715 716 717 718 719 720 721 722 723 724 725 726 727 728 729 730 731 732 733 734 735 736 737 738 739 740 741 742 743 744 745 746 747 748 749 750 751 752 753 754 755 756 757 758 759 760 761 762 763 764 765 766 767 768 769 770 771 772 773 774 775 776 777 778 779 780 781 782 783 784 785 786 787 788 789 790 791 792 793 794 795 796 797 798 799 800 801 802 803 804 805 806 807 808 809 810 811 812 813 814 815 816 817 818 819 820 821 822 823 824 825 826 827 828 829 830 831 832 833 834 835 836 837 838 839 840 841 842 843 844 845 846 847 848 849 850 851 852 853 854 855 856 857 858 859 860 861) => nil

  (count (calculate-seat-ids "resources/day5/aoc-input1.txt"))

  (range 1 10)
  #_=> (1 2 3 4 5 6 7 8 9)

  (find-missing-seat-id "resources/day5/aoc-input1.txt")
  #_=> 633
  )
