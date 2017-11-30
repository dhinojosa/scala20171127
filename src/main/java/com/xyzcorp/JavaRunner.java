package com.xyzcorp;

import java.util.Comparator;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntUnaryOperator;
import java.util.stream.IntStream;

public class JavaRunner {


    public static void main(String[] args) {
        Comparator<String> comparator = (o1, o2) -> 0;

        Function<String, Integer> f = s -> s.length();


        Integer integer = IntStream.range(1, 10000000)
                                   .map(operand -> {
                                       try {
                                           Thread.sleep(5000);
                                       } catch (InterruptedException e) {
                                           e.printStackTrace();
                                       }
                                       return operand * 1000;
                                   }).boxed().findFirst().orElse(-1);

        System.out.println(integer);

    }
}
