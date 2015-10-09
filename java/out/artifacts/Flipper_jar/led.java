public class led {

	public static void main(String[] args) {
		
		Flipper.attach(Flipper.SOURCE_USB);
		
		Flipper.setLed(0, 0, 0);
		
	}

}