(* Bug noticed by Pin-Chin Huang *)

fun(int x, int y)
{
  print(i);
  return (0+0);
}

main()
{
  int i;
  i = 1;

  fun(i = 2, i = i+1);

  print(i);
}

