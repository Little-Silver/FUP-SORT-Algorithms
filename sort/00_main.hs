import bubble_sort
import 02_insertion_sort
import 03_selection_sort
import 04_quick_sort
import 05_merge_sort
import 06_permutation_sort
import 07_binarytree_sort

main = do
    start <- getCurrentTime
    evaluate (bsort list1)
    end <- getCurrentTime
    print (diffUTCTime end start)