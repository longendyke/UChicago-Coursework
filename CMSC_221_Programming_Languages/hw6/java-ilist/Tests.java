public class Tests {

    public static void main(String[] args) {

	System.out.println("IList tests.");

	IList<String> list0 = new Nil<String>();
	IList<String> list1 = new Cons<String>("Z",list0);
	IList<String> list2 = new Cons<String>("Y",list1);
	IList<String> list3 = new Cons<String>("X",list2);

	System.out.println("list0: " + list0);
	System.out.println("list1: " + list1);
	System.out.println("list2: " + list2);
	System.out.println("list3: " + list3);

	IList<Integer> list10 = new Nil<Integer>();
	IList<Integer> list11 = new Cons<Integer>(100,list10);
	IList<Integer> list12 = new Cons<Integer>(200,list11);
	IList<Integer> list13 = new Cons<Integer>(300,list12);

	System.out.println("list10: " + list10);
	System.out.println("list11: " + list11);
	System.out.println("list12: " + list12);
	System.out.println("list13: " + list13);

    }

}