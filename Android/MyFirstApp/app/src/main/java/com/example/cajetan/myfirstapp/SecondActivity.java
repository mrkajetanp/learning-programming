package com.example.cajetan.myfirstapp;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.widget.TextView;

import java.util.Locale;
import java.util.Random;

public class SecondActivity extends AppCompatActivity {

    private static final String TOTAL_COUNT = "total_count";

    public void showRandomNumber() {
        TextView randomView = findViewById(R.id.textview_random);
        TextView headingView = findViewById(R.id.textview_heading);

        int count = getIntent().getIntExtra(TOTAL_COUNT, 0);

        Random random = new Random();
        int randomInt = 0;

        if (count > 0)
            randomInt = random.nextInt(count);

        randomView.setText(String.format(Locale.getDefault(), "%d", randomInt));
        headingView.setText(getString(R.string.random_heading, count));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_second);
        showRandomNumber();
    }
}
