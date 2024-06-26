f12 x = seqn
  where
    seqn = x : 2 * x : x * x : map sumFun zipped -- в начале задается три
                                                 -- элемента списка
    zipped = zipWith (,) (tail tl) (zipWith (,) seqn tl) -- здесь создается
                                                         -- список из тройной 
                                                         -- вложенной tuple
    tl = tail seqn        -- вспомогательная переменная для облегчения чтения
    sumFun (a, (b, c)) = digits a + digits b + digits c -- функция для map
                                                        -- складывает цифры
                                                        -- трех чисел из
                                                        -- сшитой tuple
    digits 0 = 0                            -- функция суммирует цифры числа
    digits a = mod a 10 + digits (div a 10) -- нужно первую цифру сложить с 
                                            -- суммой оставшихся цифр
 
main = do
  print ("f12 17")
  print (take 10 (f12 17))
  print ("f12 4")
  print (take 10 (f12 4))
  print ("f12 1")
  print (take 10 (f12 1))
