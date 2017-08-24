package com.example.cajetan.myfirstapp;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

/**
 * Created by cajetan on 01/07/2017.
 * Simple Fragment class
 */

public class ArticleFragment extends Fragment {
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup containter,
                             Bundle savedInstanceState) {

        // inflate the layout for this fragment
        return inflater.inflate(R.layout.article_view, containter, false);
    }
}
