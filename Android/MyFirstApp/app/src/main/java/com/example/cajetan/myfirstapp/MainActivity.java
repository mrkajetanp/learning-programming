package com.example.cajetan.myfirstapp;

import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import java.util.Locale;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class MainActivity extends AppCompatActivity {

    private static final String TOTAL_COUNT = "total_count";

    /**
     * Show a toast
     * @param view -- the view that is clicked
     */
    public void toastMe(View view) {
        Toast myToast = Toast.makeText(this, "Hello there", Toast.LENGTH_SHORT);
        myToast.show();
    }

    public void countMe(View view) {
        TextView showCountTextView = findViewById(R.id.textView);
        String countString = showCountTextView.getText().toString();
        Integer newCount = Integer.parseInt(countString) + 1;
        showCountTextView.setText(String.format(Locale.getDefault(), "%d", newCount));
    }

    public void randomMe(View view) {
        Intent randomIntent = new Intent(this, SecondActivity.class);

        TextView showCountTextView = findViewById(R.id.textView);
        int count = Integer.parseInt(showCountTextView.getText().toString());
        randomIntent.putExtra(TOTAL_COUNT, count);

        startActivity(randomIntent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }
}
