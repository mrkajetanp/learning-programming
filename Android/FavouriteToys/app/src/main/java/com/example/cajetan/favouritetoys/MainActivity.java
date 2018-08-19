package com.example.cajetan.favouritetoys;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.widget.TextView;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        TextView myToysList = findViewById(R.id.toy_names);

        for (String name : ToyBox.getToyNames())
            myToysList.append(name + "\n\n\n");
    }
}
